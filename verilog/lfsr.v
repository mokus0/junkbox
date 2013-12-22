// Linear feedback shift register
module lfsr(clk, rst, in, out);
    parameter WIDTH  = 16;
    parameter POLY   = 16'h8005;
    parameter INIT   = 0;
    parameter DIRECT = 0;
    
    input clk, rst, in;
    output reg [0 : WIDTH-1] out = INIT;
    
    always @(posedge clk or posedge rst)
    if (rst)         out = INIT;
    else if (DIRECT) out = (out << 1) ^ ((in ^ out[0]) ? POLY : 0);
    else             out = (in | out << 1) ^ (out[0] ? POLY : 0);
    
//    always @(posedge clk)
//    $display("\t[%00b]", out);
    
endmodule