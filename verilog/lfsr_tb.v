module test;
    defparam RNG.WIDTH  = 32;
    defparam RNG.POLY   = 32'h_3C1835C5; // bit-reversed A3AC183C;
    defparam RNG.INIT   = 32'h_00000001;
    
    reg clk = 0;
    always #5 clk = !clk;
    
    wire [0:31] rand;
    lfsr RNG(clk, 0, 0, rand);
    
    wire [7:0] rand_byte;
    genvar i;
    for (i = 0; i < 8; i = i + 1) assign rand_byte[i] = rand[i];
    
    initial $monitor("[%8d] %00x %00b %3d", $time, rand, rand, rand_byte);
    initial #10000 $finish;
    
endmodule
