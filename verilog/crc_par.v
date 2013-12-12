module crc_par(clk, rst, in_byte, crc);
    
    parameter CRC_WIDTH         = 16;
    parameter INIT              = 16'h_ffff;
    parameter POLY              = 16'h_1021;
    parameter DIRECT            = 1;
    parameter IN_WIDTH          = 8;
    
    input clk, rst;
    input [1:IN_WIDTH] in_byte;
    output reg[1:CRC_WIDTH] crc = INIT;
    
    wire [1:CRC_WIDTH] temp [0:IN_WIDTH];
    assign temp[0] = crc;
    
    genvar i;
    generate
        for (i = 0; i < IN_WIDTH; i = i + 1)
        begin : layer
        if (DIRECT)
            assign temp[i + 1] = (temp[i] << 1) ^ ((in_byte[i + 1] ^ temp[i][1]) ? POLY : 0);
        else
            assign temp[i + 1] = (in_byte[i + 1] | temp[i] << 1) ^ (temp[i][1] ? POLY : 0);
        end
    endgenerate
    
    always @(posedge clk or posedge rst) 
    if (rst) crc <= INIT;
    else        crc <= temp[IN_WIDTH];
    
endmodule