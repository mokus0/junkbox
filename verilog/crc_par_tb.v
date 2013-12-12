module test;
    // CRC-CCITT:
    parameter REVERSE_IN    = 0;
    defparam C0.DIRECT      = 1;
    defparam C0.INIT        = 16'h_ffff;
    defparam C0.POLY        = 16'h_1021;
    parameter REVERSE_OUT   = 0;
    parameter XOR_OUT       = 16'h_0000;

    reg clk = 0;
    always #5 clk = !clk;
    
    reg write = 0;
    wire            crc_enable;
    wire [7 :0]     crc_in;
    wire [15:0]     crc_out;
    crc_par C0(clk & crc_enable, 0, crc_in, crc_out);
    
    // some arbitrary data to feed in
    parameter DATA_LEN       = 160;
    reg [0:DATA_LEN-1] data  = 160'h_deadbeef_00000000_12345678_90abcdef_0000a5a5;
    
    // parameter DATA_LEN      = 72;
    // reg [0:DATA_LEN-1] data = 72'h_313233343536373839;
    
    if (REVERSE_IN)
    begin
        genvar j;
        for (j = 0; j < 8; j = j + 1)
        assign crc_in[j] = data[j];
    end
    else assign crc_in = data[0:7];
    assign crc_enable = 8*i < DATA_LEN;
    
    reg [0:7] i = 0;
    always @(posedge clk)
    begin
        if (8*i >= DATA_LEN)
        begin
            $display("CRC: %00x", (REVERSE_OUT ? crc_rev : crc_out) ^ XOR_OUT);
            $finish;
        end
        
        data    = data << 8;
        i       = i + 1;
    end
    
    wire [0:15] crc_rev;
    genvar j;
    for (j = 0; j < 16; j = j + 1) assign crc_rev[j] = crc_out[j];
    
    initial $monitor("[%8d] i: %d, data: %00x, crc_in: %00x, l0: %00x, crc_out: %00x, crc_rev: %00x",
        $time, i, data, crc_in, C0.temp[1], crc_out, crc_rev);

endmodule