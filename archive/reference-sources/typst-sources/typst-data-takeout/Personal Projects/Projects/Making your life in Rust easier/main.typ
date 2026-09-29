#import "@preview/touying:0.3.2": *

// Themes: default, simple, metropolis, dewdrop, university, aqua
#let s = themes.metropolis.register(aspect-ratio: "16-9")
#let s = (s.methods.info)(
  self: s,
  title: [Making your life in Rust easier],
  subtitle: [+proc macro case study],
  author: [Lukáš Hozda],
  date: datetime.today(),
  institution: [discord: *\@magnusi*],
)
#let (init, slides, touying-outline, alert) = utils.methods(s)
#show: init

#show strong: alert

#let (slide, title-slide, focus-slide) = utils.slides(s)
#show: slides


= Effort in Rust

- Rust has some reputation for requiring a lot of effort to develop in
- Not necessarily true
- Let's look at some tools that can make our life significantly easier

= Toolchain-level

- `cargo fix [--clippy] [--all-features]` 
  - automatically fix some simple lints/warnings
  - will never fix anything that could  affect the behavior of your program
- `cargo update` and from `cargo-edit`: `cargo upgrade`
  - easily update your dependencies
- `cargo generate-lockfile`
  - generate a completely new lockfile

= Library-level

- Rayon
  - Easily make your iterator-processing code parallel
- Itertools
  - Tons of useful iterator-related tools
- `derive_more`
  - Derive STD traits that you would otherwise need to implement yourself
- `thiserror`
  - Generate concrete Error types
- `Anyhow`
  - Dynamic Error type that you can stick a context to
- `strum`
  - Tools work working with strings and enums


== Rayon

```rust
use rayon::prelude::*;

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    let sum: i32 = numbers.par_iter().sum();
    println!("Sum: {}", sum);
}
```

== Itertools

```rust
use itertools::Itertools;

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    let combinations = numbers.iter().combinations(3);
    for combo in combinations {
        println!("{:?}", combo);
    }
}
```

== derive_more

```rs
use derive_more::{Add, Display, From};

#[derive(Add, Display, From, Copy, Clone)]
#[display(fmt = "({}, {})", x, y)]
struct Point {
    x: i32,
    y: i32,
}
fn main() {
    let p1 = Point { x: 1, y: 2 };
    let p2 = Point { x: 3, y: 4 };
    let p3 = p1 + p2;
    println!("p1: {}", p1);
    println!("p2: {}", p2);
    println!("p3: {}", p3);
    let p4 = Point::from((5, 6)); println!("p4: {}", p4);
}

```

== thiserror

```rust
use thiserror::Error;
#[derive(Error, Debug)]
enum MyError {
    #[error("Invalid input: {0}")] InvalidInput(String),
    #[error("IO error: {0}")] IoError(#[from] std::io::Error),
}

fn process_input(input: &str) -> Result<(), MyError> {
    if input.is_empty() {
        return Err(MyError::InvalidInput("Input cannot be empty".to_string()));
    }
    Ok(())
}
fn main() {
    if let Err(err) = process_input("") { println!("Error: {}", err); }
}
```

== anyhow

```rust
use anyhow::Result;

fn divide(a: i32, b: i32) -> Result<i32> {
    if b == 0 {
        anyhow::bail!("Cannot divide by zero");
    }
    Ok(a / b)
}

fn main() -> Result<()> {
    let result = divide(10, 2)?;
    println!("Result: {}", result);
    Ok(())
}
```

== anyhow w/ context

```rust
use anyhow::{Context, Result};

fn read_config(file_path: &str) -> Result<String> {
    std::fs::read_to_string(file_path)
        .with_context(|| format!("Failed to read config file: {}", file_path))
}

fn main() -> Result<()> {
    let config = read_config("config.txt")?;
    println!("Config: {}", config);
    Ok(())
}
```

== strum

```rust
use strum_macros::{Display, EnumString};

#[derive(Display, EnumString)]
enum Color {
    #[strum(serialize = "red")] Red,
    #[strum(serialize = "green")] Green,
    #[strum(serialize = "blue")] Blue,
}
fn main() {
    let color = Color::Red; println!("Color: {}", color);
    let color_str = "green";
    if let Ok(parsed_color) = Color::from_str(color_str) {
        println!("Parsed color: {}", parsed_color);
    } else {
        println!("Invalid color string: {}", color_str);
    }
}
```

= Language-level

- Reduce ifs with constant patterns
- Destructure in parameters
- Don't be afraid of generics
- Use deref coercion when writing containers/smart_pointers
- Write macros (even proc macros)

== Reduce control structures with constant patterns

```rust
struct Owner {
    name: String,
    age: u8,
}
enum Color {
    Blue, Red,
}
struct CarFeatures {
    color: Color,
    make: String,
}
struct OwnedCar {
    spz: String,
    features: CarFeatures,
    owner: Owner,
}

fn print_blue_car_owner(car: Option<OwnedCar>) {
    use Color::Blue;
    if let Some(OwnedCar {
        features: CarFeatures { color: Blue, .. },
        owner: Owner { name, .. },
        ..
    }) = car
    {
        println!("{name}")
    }
}
```

== Destructure in parameters

```rust
struct Rectangle {
    width: u32,
    height: u32,
}

fn area(Rectangle { width, height }: &Rectangle) -> u32 {
    width * height
}

fn main() {
    let rect = Rectangle { width: 10, height: 20 };
    println!("Area: {}", area(&rect));
}
```

== Declarative macro - compile-time list

Usage:
```rs
fn make_list() -> list!(i32, i32, i32) {
    unimplemented!()
}
```

Should generate type `Node<i32, Node<i32, Node<i32, End>>>`

== List macro impl

```rs
macro_rules! list {
    () => {
        End
    };
    ($x:ty) => {
        Node<$x, End>
    };
    ($x:ty, $($xs:ty),+) => {
        Node<$x, list!($($xs),+)>
    };
}
```

== Or maybe just a type alias

```rs
#[macro_export]
macro_rules! warp_filter {
    () => {
        impl Filter<Extract = impl Reply, Error = Rejection> + Clone
    };
}
```

= Proc macro usecase

I want to be able to do this:

```rs
taproute! {
    POST login(): LoginRequest -> [login] -> LoginResponse;
    |body: LoginRequest| {
        let user = User::find_by_username(&body.username).await;

        match dbg!(user) {
            Some(user) if user.verify_password(&body.password) => {
                let token = generate_token(user.user_id);
                crate::ok(&LoginResponse { token })
            }
            _ => crate::unauthorized(&"Wrong password"),
        }
    }
}
```
== 
Which will generate three methods:
- route() - defines the route for Warp framework
- handler() - produces the response of this endpoint
- call() - a function for my Rust WASM frontend that calls the endpoint

==

- First, the dependencies:
  - `proc_macro2` - needed for interacting with proc macro libraries
  - `quote` - so I can cleanly generate tokens
  - `syn` - so I can parse input tokens

== Some types

```rs
use syn::{
  Ident, Pat, Type,
};

enum RouteSegment {
    Static(Ident),
    Dynamic(Ident),
}

struct HandlerParam {
    pat: Pat,
    ty: Type,
}
```
==
```rust
impl ToTokens for HandlerParam {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.pat.to_tokens(tokens);
        tokens.extend(quote! { : });
        self.ty.to_tokens(tokens);
    }
}
```
==
```rust
impl ToTokens for RouteSegment {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            RouteSegment::Static(s) => {
                tokens.extend(quote! { ::warp::path(#s) })
            }
            RouteSegment::Dynamic(ident) => tokens.extend(
                quote! { ::warp::path::param::<#ident>() },
            ),
        }
    }
}
```

== 

```rs
use syn::{
    Block, Ident, Type,
};

struct TaprouteInput {
    auth: Option<Ident>,
    method: Ident,
    name: Ident,
    input_type: Option<proc_macro2::TokenStream>,
    output_type: Type,
    route_segments: Vec<RouteSegment>,
    handler_params: Vec<HandlerParam>,
    handler_body: Block,
}
```

== I can implement parse for TapRouteInput

```rs
impl syn::parse::Parse for TaprouteInput {
    fn parse(
        input: syn::parse::ParseStream,
    ) -> syn::Result<Self> {
        // ...
    }
}
```

==

```rust
        let first_ident = input.parse::<Ident>()?;

        let auth: Option<Ident>;
        let method: Ident;

        if first_ident == "auth" {
            auth = Some(first_ident);
            method = input.parse::<Ident>()?;
        } else {
            auth = None;
            method = first_ident;
        }

        let name = input.parse()?;
```

==

```rust
        let _content;
        syn::parenthesized!(_content in input);
        input.parse::<Token![:]>()?;
```

==

```rs
        let mut input_type = None;
        if input.peek(syn::Ident) {
            let ty = input.parse::<Type>()?;
            input_type = Some(
                quote! { .and(::warp::body::json::<#ty>()) },
            );
            input.parse::<Token![->]>()?;
        }
```

==

```rs
        let route_segments = parse_route_segments(input)?;
        input.parse::<Token![->]>()?;
        let output_type = input.parse()?;
        input.parse::<Token![;]>()?;

        let handler_params = parse_handler_params(input)?;
        let handler_body = input.parse()?;
```

==

```rs
        Ok(TaprouteInput {
            auth,
            method,
            name,
            input_type,
            output_type,
            route_segments,
            handler_params,
            handler_body,
        })
```

== parse_route_segments()

```rs
fn parse_route_segments(
    source: syn::parse::ParseStream,
) -> syn::Result<Vec<RouteSegment>> {
    let mut segments = Vec::new();

    let input;
    bracketed!(input in source);

    while !input.is_empty() {
        if input.peek(syn::Ident) {
            segments.push(RouteSegment::Static(input.parse()?));
        } else if input.peek(Token![/]) {
            input.parse::<Token![/]>()?;
        } else if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;
            let ident = input.parse()?;
            input.parse::<Token![=]>()?;
            segments.push(RouteSegment::Dynamic(ident));
        } else {
            break;
        }
    }

    Ok(segments)
}
```

== parse_handler_params()

```rust
fn parse_handler_params(
    input: syn::parse::ParseStream,
) -> syn::Result<Vec<HandlerParam>> {
    let mut params = Vec::new();

    input.parse::<Token![|]>()?;
    while !input.peek(Token![|]) {
        let pat_type: syn::PatType = input.parse()?;
        params.push(HandlerParam {
            pat: *pat_type.pat,
            ty: *pat_type.ty,
        });

        if input.peek(Token![,]) {
            input.parse::<Token![,]>()?;
        } else {
            break;
        }
    }
    input.parse::<Token![|]>()?;

    Ok(params)
}
```

==

```rust
#[proc_macro]
pub fn taproute(input: TokenStream) -> TokenStream {
  // ...
}
```

==

```rust
    let input = parse_macro_input!(input as TaprouteInput);
    let auth = input.auth;
    let method = input.method;
    let name = input.name;
    let input_type = input.input_type;
    let output_type = input.output_type;
    let route_segments = input.route_segments;
    let handler_params = input.handler_params;
    let handler_body = input.handler_body;
```

==

```rust
let url_str = generate_url_str(&route_segments);

fn generate_url_str(segments: &[RouteSegment]) -> String {
    let mut url_str = String::new();

    for segment in segments {
        match segment {
            RouteSegment::Static(s) => {
                url_str.push_str("/");
                url_str.push_str(&s.to_string())
            }
            RouteSegment::Dynamic(_) => url_str.push_str("/{}"),
        }
    }

    url_str
}
```

==

```rust
    let has_auth = auth.is_some();
    let call_params =
        generate_call_params(&handler_params, has_auth);
```
==

```rust
fn generate_call_params(
    handler_params: &[HandlerParam],
    has_auth: bool,
) -> Vec<proc_macro2::TokenStream> {
    handler_params .iter()
        .filter(|param| {
            if let Pat::Ident(pat_ident) = &param.pat {
                pat_ident.ident != "user_id"
            } else { true }
        })
        .map(|param| {
            let pat = &param.pat;
            let ty = &param.ty;
            quote! { #pat: &#ty }
        })
        .collect()
}
```
==

```rust
    let has_body = input_type.is_some();
    let call_args =
        generate_call_args(&handler_params, has_body, has_auth);
```
==

```rust
fn generate_call_args(
    handler_params: &[HandlerParam],
    has_body: bool,
    has_auth: bool,
) -> Vec<proc_macro2::TokenStream> {
    let iterator = &handler_params[0..handler_params.len()
        - if has_body { 1 } else { 0 }
        - if has_auth { 1 } else { 0 }]
        .to_vec();
    iterator
        .iter()
        .map(|param| {
            let pat = &param.pat;
            quote! { #pat }
        })
        .collect()
}
```
==

```rust
    let smol_method = Ident::new(
        &method.to_string().to_lowercase(),
        method.span(),
    );

    let warp_routes: proc_macro2::TokenStream =
        generate_warp_routes(&route_segments).into();
```
==

```rust
fn generate_warp_routes( segments: &[RouteSegment]) -> TokenStream {
    let routes = segments.into_iter().fold(quote!(), |acc, segment| {
            match segment {
                RouteSegment::Static(ident) => {
                    let name = ident.to_string(); quote! {
                        #acc
                        .and(warp::path(#name))
                    }
                },
                RouteSegment::Dynamic(_ident) =>
                    quote! {
                        #acc
                        .and(warp::path::param())
                    },
            }});
    TokenStream::from(quote! { #routes })
}
```

==

```rust
    let auth_filter = if auth.is_some() {
        quote! { .and(super::jwt::with_auth()) }
    } else {
        quote! {}
    };
```

==

```rust
    let auth_fr_init = if auth.is_some() {
        quote! {
            let token = ::web_sys::window()
                .unwrap()
                .local_storage()
                .unwrap()
                .unwrap()
                .get_item("token")
                .unwrap()
                .unwrap();
        }
    } else {
        quote! {}
    };
```

==

```rust
    let auth_fr_include = if auth.is_some() {
        quote! {
            .bearer_auth(token)
        }
    } else {
        quote! {}
    };
```

==

```rust
    let yes_json = if input_type.is_some() {
        let body_name = handler_params.last().unwrap();
        let body_name = &body_name.pat;
        quote! { .json(&#body_name) }
    } else {
        quote! {}
    };
```

==

```rust
let expanded = quote! {
  mod #name {
  // ..
  }
}
```

==

```rust
use super::*;
#[cfg(feature = "backend")]
use warp::{Filter, Reply, Rejection};

#[cfg(feature = "backend")]
pub fn route() -> impl ::warp::Filter<Extract = impl Reply, Error = Rejection> + Clone {
    ::warp::#smol_method()
        #warp_routes
        .and(::warp::path::end())
        #auth_filter
        #input_type
        .and_then(handler)
        .with(warp::filters::trace::named(stringify!(#name)))
}
```

==

```rust
#[cfg(feature = "backend")]
pub async fn handler(#(#handler_params),*) -> Result<impl Reply, Rejection> {
    #handler_body
}
```

==

```rust
            #[cfg(feature = "frontend")]
            pub async fn call(#(#call_params),*) -> ::anyhow::Result<#output_type> {
                #auth_fr_init

                let base_url: String = web_sys::window().unwrap().origin();

                let client = ::reqwest::Client::new();
                let url = ::std::format!(#url_str, #(#call_args),*);
                let response = client
                    .request(::reqwest::Method::#method, &(base_url + url.as_str()))
                    #auth_fr_include
                    #yes_json
                    .send()
                    .await?;

                let result = response.json().await?;
                ::anyhow::Result::Ok(result)
            }
```

==

```rust
    TokenStream::from(expanded)
```
