use nom::{Err, IResult, Offset};
use std::io::Read;

pub struct ParseBuffer<R, P> {
    pub buf: circular::Buffer,
    pub read: R,
    pub parse: P,
}

use nom::error::VerboseError;

impl<O, R: Read, P> Iterator for ParseBuffer<R, P>
where
    P: Fn(&[u8]) -> IResult<&[u8], O, VerboseError<&[u8]>>,
{
    type Item = O;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match (self.parse)(self.buf.data()) {
                Err(Err::Incomplete(_)) => {
                    // ensure that we have some space available in the buffer
                    if self.buf.available_space() == 0 {
                        if self.buf.position() == 0 {
                            //println!("growing buffer capacity");

                            // double buffer capacity
                            self.buf.grow(self.buf.capacity() * 2);
                        } else {
                            self.buf.shift();
                        }
                    }

                    // read from file to free space of buffer
                    let read_bytes = self.read.read(self.buf.space()).expect("should write");
                    self.buf.fill(read_bytes);
                    //println!("Read {} bytes from file", read_bytes);

                    if self.buf.available_data() == 0 {
                        // no more data to read or parse, stopping the reading loop
                        break None;
                    } else if read_bytes == 0 {
                        panic!("incomplete parse at end of file");
                    }
                }

                Err(Err::Error(e)) | Err(Err::Failure(e)) => panic!("parse error: {:#?}", e),

                Ok((remaining, toplevel)) => {
                    self.buf.consume(self.buf.data().offset(remaining));
                    break Some(toplevel);
                }
            }
        }
    }
}
