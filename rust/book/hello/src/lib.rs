use std::thread;
use std::sync::mpsc;
use std::sync::Mutex;
use std::sync::Arc;

enum Message {
    NewJob(Job),
    Terminate,
}

pub struct ThreadPool {
    workers: Vec<Worker>,
    chan: mpsc::Sender<Message>,
}

trait FnBox {
    fn call_box(self: Box<Self>);
}

impl<F: FnOnce()> FnBox for F {
    fn call_box(self: Box<F>) {
        (*self)()
    }
}

type Job = Box<FnBox + Send + 'static>;

impl ThreadPool {
    pub fn new(size: usize) -> ThreadPool {
        assert! (size > 0);

        let (chan, receiver) = mpsc::channel();

        let receiver = Arc::new(Mutex::new(receiver));
        
        let mut workers = Vec::with_capacity(size);
        
        for id in 0..size {
            workers.push(Worker::new(id, receiver.clone()));
        }
        
        ThreadPool {
            workers,
            chan,
        }
    }

    pub fn execute<F> (&self, f: F)
        where F: FnOnce() + Send + 'static
    {
        let job = Box::new(f);

        self.chan.send(Message::NewJob(job)).unwrap();
    }
}

impl Drop for ThreadPool {
    fn drop(&mut self) {
        for _ in &mut self.workers {
            self.chan.send(Message::Terminate).unwrap();
        }
        
        for worker in &mut self.workers {
            println!("Shutting down worker {}", worker.id);

            if let Some(thread) = worker.thread.take() {
                thread.join().unwrap();
            }
        }
    }
}

struct Worker {
    id: usize,
    thread: Option<thread::JoinHandle<()>>,
}

impl Worker {
    fn new(id: usize, receiver: Arc<Mutex<mpsc::Receiver<Message>>>) -> Worker {
        let thread = thread::spawn(move || {
            loop {
                let msg = receiver.lock().unwrap().recv().unwrap();

                match msg {
                    Message::NewJob(job) => {
                        println!("Worker {} got a job; executing.", id);
                        job.call_box();
                    },
                    Message::Terminate   => {
                        println!("Worker {} done; terminating.", id);
                        break;
                    },
                };
            }
        });

        Worker {
            id,
            thread: Some(thread),
        }
    }
}
