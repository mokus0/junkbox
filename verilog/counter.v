module counter(clk, rst, out);
    
    parameter WIDTH = 8;
    
    input                       clk, rst;
    output reg [WIDTH-1 : 0]    out = 0;
    
    always @(posedge clk or posedge rst)
    out <= rst ? 0 : out + 1;
    
endmodule // counter