// bytewise wrapper for lfsr.  accumulates CRC of a byte in 8 clk cycles,
// indicating completion (and readiness for next byte) by setting rdy to 1.
// "accept" pulses 1 during the cycle in which a byte is accepted.
module crc(clk, write, in, accept, rdy, out);
    // default to CRC-CCITT
    parameter WIDTH         = 16;
    parameter DIRECT        = 1;
    parameter INIT          = 16'h_ffff;
    parameter POLY          = 16'h_1021;
    parameter BYTE_SIZE     = 8;
    
    input wire                  clk, write;
    input wire[1:BYTE_SIZE]     in;
    
    output wire                 rdy;
    assign                      rdy     = ~(in_mask[1]);
    output wire                 accept;
    assign                      accept  = in_mask[BYTE_SIZE];
    output wire[1:WIDTH]        out;
    
    
    // data is moved through internal register on positive clock edges.
    // if data is present in register, LFSR is clocked with the negation of 
    // clk, causing CRC to update on negative edges.
    reg [1:BYTE_SIZE]   lfsr_in, in_mask = 0;
    wire                lfsr_clk         = in_mask[1] & ~clk;
    
    defparam SR.WIDTH  = WIDTH;
    defparam SR.POLY   = POLY;
    defparam SR.INIT   = INIT;
    defparam SR.DIRECT = DIRECT;
    lfsr SR(lfsr_clk, 0, lfsr_in[1], out);
    
    always @(posedge clk)
    begin
        // shift next bit for LFSR if byte in progress
        if (~rdy)
        begin
            in_mask         = in_mask << 1;
            lfsr_in         = lfsr_in << 1;
        end
        
        // read in next byte if LFSR and new byte are both available.
        // note that this can occur in the same cycle as the previous byte finished.
        if (rdy && write)
        begin
            in_mask         = ~0;
            lfsr_in         = in;
        end
    end

endmodule
