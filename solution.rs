use tokio::net::{TcpListener, TcpStream};
use tokio::net::tcp::OwnedWriteHalf;
use tokio::io::{AsyncRead, AsyncWrite};
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::sync::Mutex;
use tokio::task::JoinHandle;
use log::{info, error};
use std::convert::TryInto;
use std::sync::Arc;
use std::fmt;

#[derive(Debug)]
pub enum Error {
    IOError(std::io::Error),
    Utf8Error(std::string::FromUtf8Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for Error {
}

impl From<std::io::Error> for Error {
    fn from(error: std::io::Error) -> Self {
        Error::IOError(error)
    }
}

impl From<std::string::FromUtf8Error> for Error {
    fn from(error: std::string::FromUtf8Error) -> Self {
        Error::Utf8Error(error)
    }
}

async fn send_string(stream: &mut (impl AsyncWrite + Unpin), string: &str) -> Result<(), Error> {
    let data = string.as_bytes();
    let size = data.len().try_into().unwrap_or(u16::MAX);
    stream.write_u16(size).await?;
    stream.write_all(&data[..(size as usize)]).await?;
    Ok(())
}

async fn receive_string(stream: &mut (impl AsyncRead + Unpin)) -> Result<String, Error> {
    let size = stream.read_u16().await?;
    let mut data = vec![0; size.into()];
    stream.read_exact(&mut data).await?;
    let message = String::from_utf8(data)?;
    Ok(message)
}

pub struct Client {
    backend: Arc<Mutex<ClientBackend>>,
}

impl Client {
    pub fn new(stream: TcpStream) -> Self {
        let messages = Vec::new();
        let backend = Arc::new(Mutex::new(ClientBackend { stream, messages }));
        Client { backend }
    }

    pub async fn channel(&self, name: &str) -> Result<Channel, Error> {
        let name = name.to_string();
        let backend = self.backend.clone();
        let channel = Channel { name, backend };
        Ok(channel)
    }
}

struct ClientBackend {
    stream: TcpStream,
    messages: Vec<(String, String)>,
}

pub struct Channel {
    name: String,
    backend: Arc<Mutex<ClientBackend>>,
}

impl Channel {
    pub async fn send(&self, message: &str) -> Result<(), Error> {
        let backend = &mut *self.backend.lock().await;
        send_string(&mut backend.stream, &self.name).await?;
        send_string(&mut backend.stream, message).await?;
        Ok(())
    }

    pub async fn receive(&self) -> Result<String, Error> {
        let backend = &mut *self.backend.lock().await;
        if let Some(p) = backend.messages.iter().position(|(c, _)| c == &self.name) {
            let (channel, message) = backend.messages.remove(p);
            assert_eq!(channel, self.name);
            return Ok(message);
        }
        loop {
            let channel = receive_string(&mut backend.stream).await?;
            let message = receive_string(&mut backend.stream).await?;
            if channel == self.name {
                return Ok(message);
            } else {
                backend.messages.push((channel, message));
            }
        }
    }
}

pub struct Server {
    task: JoinHandle<()>,
    backend: Arc<Mutex<ServerBackend>>,
}

impl Server {
    pub fn new(listener: TcpListener) -> Self {
        let senders = Vec::new();
        let tasks = Vec::new();
        let backend = Arc::new(Mutex::new(ServerBackend { senders, tasks }));
        let task = tokio::spawn({
            let backend = backend.clone();
            async move { run(backend, listener).await.unwrap() }
        });
        Server { task, backend }
    }

    pub async fn quit(self) {
        info!("Server quits");
        let mut backend = self.backend.lock().await;
        for (sender, _) in &mut backend.senders {
            sender.shutdown().await.ok();
        }
        self.task.abort();
        self.task.await.ok();
        for task in &mut backend.tasks {
            task.abort();
            task.await.ok();
        }
    }
}

#[derive(Copy, Clone, PartialEq)]
struct Token(usize);

struct ServerBackend {
    senders: Vec<(OwnedWriteHalf, Token)>,
    tasks: Vec<JoinHandle<()>>,

}

async fn run(backend: Arc<Mutex<ServerBackend>>, listener: TcpListener) -> Result<(), std::io::Error> {
    let mut token = Token(0);
    loop {
        let (stream, _) = listener.accept().await?;
        let task = tokio::spawn({
            let backend = backend.clone();
            token.0 += 1;
            async move {
                match handle(backend.clone(), stream, token).await {
                    Ok(()) => (),
                    Err(e) => error!("{:?}", e),
                }
            }
        });
        backend.lock().await.tasks.push(task);
    }
}

async fn handle(backend: Arc<Mutex<ServerBackend>>, stream: TcpStream, token: Token) -> Result<(), Error> {
    info!("Accepted a client.");
    let (mut receiver, sender) = stream.into_split();
    backend.lock().await.senders.push((sender, token));
    loop {
        let channel = receive_string(&mut receiver).await?;
        let message = receive_string(&mut receiver).await?;
        info!("Broadcasting a message.");
        broadcast(channel, message, &backend, token).await?;
    }
}

async fn broadcast(channel: String, message: String, backend: &Arc<Mutex<ServerBackend>>, skip: Token) -> Result<(), Error> {
    for (sender, token) in &mut backend.lock().await.senders {
        if *token != skip {
            send_string(sender, &channel).await?;
            send_string(sender, &message).await?;
        }
    }
    Ok(())
}
