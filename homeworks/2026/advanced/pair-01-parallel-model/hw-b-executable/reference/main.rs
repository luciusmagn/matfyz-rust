use std::io::{self, Read};

#[derive(Debug)]
struct Job {
    id: i64,
    values: Vec<i64>,
}

#[derive(Debug)]
struct JobResult {
    id: i64,
    sum: i64,
    checksum: i64,
}

fn parse_jobs(input: &str) -> (usize, Vec<Job>) {
    let mut parts = input.split_whitespace();

    let workers = parts
        .next()
        .expect("missing workers")
        .parse::<usize>()
        .expect("invalid workers");

    let job_count = parts
        .next()
        .expect("missing job_count")
        .parse::<usize>()
        .expect("invalid job_count");

    let mut jobs = Vec::with_capacity(job_count);
    for _ in 0..job_count {
        let id = parts
            .next()
            .expect("missing job id")
            .parse::<i64>()
            .expect("invalid job id");

        let value_count = parts
            .next()
            .expect("missing value_count")
            .parse::<usize>()
            .expect("invalid value_count");

        let mut values = Vec::with_capacity(value_count);
        for _ in 0..value_count {
            let value = parts
                .next()
                .expect("missing value")
                .parse::<i64>()
                .expect("invalid value");
            values.push(value);
        }

        jobs.push(Job { id, values });
    }

    (workers, jobs)
}

fn process_job(job: Job) -> JobResult {
    let sum = job.values.iter().copied().sum::<i64>();
    let checksum = job
        .values
        .iter()
        .enumerate()
        .map(|(idx, value)| (idx as i64 + 1) * value)
        .sum::<i64>();

    JobResult {
        id: job.id,
        sum,
        checksum,
    }
}

fn main() {
    let mut input = String::new();
    io::stdin()
        .read_to_string(&mut input)
        .expect("failed to read stdin");

    let (_workers, jobs) = parse_jobs(&input);

    let mut results = jobs.into_iter().map(process_job).collect::<Vec<_>>();
    results.sort_by_key(|result| result.id);

    let mut total_sum = 0_i64;
    let mut total_checksum = 0_i64;

    for result in results {
        println!(
            "job {}: sum={} checksum={}",
            result.id, result.sum, result.checksum
        );
        total_sum += result.sum;
        total_checksum += result.checksum;
    }

    println!("total_sum={}", total_sum);
    println!("total_checksum={}", total_checksum);
}
