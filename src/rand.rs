use rand::{Rng, SeedableRng};

/// This wraps all the operations we care about for our random numbers
/// in a convenient interfacee
pub trait MatzoRand {
    fn gen_range_i64(&mut self, min: i64, max: i64) -> i64;
    fn gen_range_usize(&mut self, min: usize, max: usize) -> usize;
}

/// This is a newtype around `ThreadRng`, and is the RNG we use if we
/// don't provide an explicit seed to the interpreter
pub struct DefaultRNG {
    rand: rand::rngs::ThreadRng,
}

impl DefaultRNG {
    pub fn new() -> DefaultRNG {
        DefaultRNG {
            rand: rand::thread_rng(),
        }
    }
}

impl Default for DefaultRNG {
    fn default() -> DefaultRNG {
        Self::new()
    }
}

impl MatzoRand for DefaultRNG {
    fn gen_range_i64(&mut self, min: i64, max: i64) -> i64 {
        self.rand.gen_range(min..max)
    }

    fn gen_range_usize(&mut self, min: usize, max: usize) -> usize {
        self.rand.gen_range(min..max)
    }
}

/// This is a newtype around `StdRng`, and is the RNG we use if we
/// _do_ provide a seed number for reproducible runs
pub struct SeededRNG {
    rand: rand::rngs::StdRng,
}

impl SeededRNG {
    pub fn from_seed(seed: u64) -> SeededRNG {
        SeededRNG {
            rand: rand::rngs::StdRng::seed_from_u64(seed),
        }
    }
}

impl MatzoRand for SeededRNG {
    fn gen_range_i64(&mut self, min: i64, max: i64) -> i64 {
        self.rand.gen_range(min..max)
    }

    fn gen_range_usize(&mut self, min: usize, max: usize) -> usize {
        self.rand.gen_range(min..max)
    }
}
