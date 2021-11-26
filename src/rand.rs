use rand::{SeedableRng, Rng};

pub trait MatzoRand {
    fn gen_range_i64(&mut self, min: i64, max: i64) -> i64;
    fn gen_range_usize(&mut self, min: usize, max: usize) -> usize;
}

pub struct DefaultRNG {
    rand: rand::rngs::ThreadRng,
}

impl DefaultRNG {
    pub fn new() -> DefaultRNG {
        DefaultRNG {
            rand: rand::thread_rng()
        }
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

pub struct SeededRNG {
    rand: rand::rngs::StdRng,
}

impl SeededRNG {
    pub fn from_seed(seed: u64) -> SeededRNG {
        SeededRNG {
            rand: rand::rngs::StdRng::seed_from_u64(seed)
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
