use crossbeam_channel::{bounded, unbounded, Receiver, Sender};
use once_cell::sync::Lazy;
use std::thread;

/// To avoid concurrency issues of Boehm GC, we need a dedicated thread for backends.
pub fn dedicated_thread() -> &'static Thread {
    static THREAD: Lazy<Thread> = Lazy::new(Thread::spawn);
    &*THREAD
}

#[derive(Debug)]
pub struct Thread {
    _handle: thread::JoinHandle<()>,
    sender: Sender<Box<dyn FnOnce() + Send + 'static>>,
}

impl Thread {
    fn spawn() -> Self {
        let (sender, receiver) = unbounded();
        let _handle = thread::spawn(move || process_tasks(receiver));
        Self { _handle, sender }
    }

    pub fn run<F, T>(&self, f: F) -> JoinHandle<T>
    where
        F: FnOnce() -> T,
        F: Send + 'static,
        T: Send + 'static,
    {
        let (sender, receiver) = bounded(0);
        self.sender
            .send(Box::new(move || {
                let _ = sender.send(f());
            }))
            .unwrap();
        JoinHandle { receiver }
    }
}

#[derive(Debug)]
pub struct JoinHandle<T> {
    receiver: Receiver<T>,
}

impl<T> JoinHandle<T> {
    pub fn join(self) -> T {
        self.receiver.recv().unwrap()
    }
}

fn process_tasks(receiver: Receiver<Box<dyn FnOnce() + Send + 'static>>) {
    while let Ok(f) = receiver.recv() {
        f();
    }
}
