use byteorder::WriteBytesExt;
use std::io;

/// Write an initial bytes of an ELF header.
pub fn write(w: &mut impl io::Write) -> io::Result<()> {
    w.write_all(&MAG)?; // 0x0-0x3
    w.write_u8(CLASS)?; // 0x4
    w.write_u8(DATA)?; // 0x5
    w.write_u8(VERSION)?; // 0x6
    w.write_u8(OSABI)?; // 0x7
    w.write_u8(ABIVERSION)?; // 0x8
    w.write_all(&PAD)?; // 0x9-0xf
    Ok(())
}

pub const MAG: [u8; 4] = [0x7f, b'E', b'L', b'F'];

pub const CLASS: u8 = 2; // always CLASS64 on x64

pub const DATA: u8 = 1; // always LSB on x64

pub const VERSION: u8 = 1;

pub const OSABI: u8 = 0;

pub const ABIVERSION: u8 = 0;

pub const PAD: [u8; 7] = [0; 7];
