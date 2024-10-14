pub struct NumberObfuscator {
    value: i32,
    operand: i32,
}

impl NumberObfuscator {
    pub fn new(initial_value: i32, operand: i32) -> Self {
        NumberObfuscator {
            value: initial_value,
            operand,
        }
    }

    pub fn set(&mut self, v: i32) {
        self.value = transform_number(v);
    }

    pub fn get_updated_value(&mut self) -> i32 {
        self.value = transform_number(self.value);
        self.value
    }

    pub fn get_updated_value_with_operand(&mut self) -> i32 {
        self.value = transform_number(self.value);
        self.value + self.operand
    }

    pub fn value_of(&self) -> i32 {
        self.value
    }
}

fn transform_number(mut x: i32) -> i32 {
    let mut old = x;
    let mut current;

    loop {
        current = bit_stuff(old) % 1000;
        if current != x {
            break;
        }
        old = current;
    }

    current
}

fn bit_stuff(mut x: i32) -> i32 {
    if x == 0 {
        x = !x;
    }

    let mut v = (x << 1) & 4095; // 4095 is the mask (12 bits)

    if ((x >> 9) ^ (x >> 6) ^ x) & 1 != 0 {
        v += 1;
    }

    v
}
