// Behavioral model of MIPS - single cycle implementation, R-types and addi
module reg_file (RR1,RR2,WR,WD,RegWrite,RD1,RD2,clock);
  input [1:0] RR1,RR2,WR;
  input [15:0] WD;
  input RegWrite,clock;
  output [15:0] RD1,RD2;
  reg [15:0] Regs[0:3];
  assign RD1 = Regs[RR1];
  assign RD2 = Regs[RR2];
  initial Regs[0] = 0;
  always @(negedge clock)
    if (RegWrite==1 & WR!=0) 
	Regs[WR] <= WD;
endmodule

// 16-bit MIPS ALU in Verilog (1-bit ALU gate-level implementation)

module alu (ALUct1,A,B,ALUOut,Zero);
   input [15:0] A;
   input [15:0] B;
   input [3:0] ALUct1;
   output [15:0] ALUOut;
   output Zero;
   //module ALU1 (a,b,ainvert,binvert,op,less,carryin,carryout,result);
   ALU1   alu0 (A[0],B[0],ALUct1[3],ALUct1[2],ALUct1[1:0],set, ALUct1[2],c1,ALUOut[0]);
   ALU1   alu1 (A[1],B[1],ALUct1[3],ALUct1[2],ALUct1[1:0],1'b0,c1,   c2,ALUOut[1]);
   ALU1   alu2 (A[2],B[2],ALUct1[3],ALUct1[2],ALUct1[1:0],1'b0,c2,   c3,ALUOut[2]);
   ALU1   alu3 (A[3],B[3],ALUct1[3],ALUct1[2],ALUct1[1:0],1'b0,c3,   c4,ALUOut[3]);
   ALU1   alu4 (A[4],B[4],ALUct1[3],ALUct1[2],ALUct1[1:0],1'b0,c4,   c5,ALUOut[4]);
   ALU1   alu5 (A[5],B[5],ALUct1[3],ALUct1[2],ALUct1[1:0],1'b0,c5,   c6,ALUOut[5]);
   ALU1   alu6 (A[6],B[6],ALUct1[3],ALUct1[2],ALUct1[1:0],1'b0,c6,   c7,ALUOut[6]);
   ALU1   alu7 (A[7],B[7],ALUct1[3],ALUct1[2],ALUct1[1:0],1'b0,c7,   c8,ALUOut[7]);
   ALU1   alu8 (A[8],B[8],ALUct1[3],ALUct1[2],ALUct1[1:0],1'b0,c8,   c9,ALUOut[8]);
   ALU1   alu9 (A[9],B[9],ALUct1[3],ALUct1[2],ALUct1[1:0],1'b0,c9,   c10,ALUOut[9]);
   ALU1   alu10 (A[10],B[10],ALUct1[3],ALUct1[2],ALUct1[1:0],1'b0,c10,   c11,ALUOut[10]);
   ALU1   alu11 (A[11],B[11],ALUct1[3],ALUct1[2],ALUct1[1:0],1'b0,c11,   c12,ALUOut[11]);
   ALU1   alu12 (A[12],B[12],ALUct1[3],ALUct1[2],ALUct1[1:0],1'b0,c12,   c13,ALUOut[12]);
   ALU1   alu13 (A[13],B[13],ALUct1[3],ALUct1[2],ALUct1[1:0],1'b0,c13,   c14,ALUOut[13]);
   ALU1   alu14 (A[14],B[14],ALUct1[3],ALUct1[2],ALUct1[1:0],1'b0,c14,   c15,ALUOut[14]);
   ALUmsb alu15 (A[15],B[15],ALUct1[3],ALUct1[2],ALUct1[1:0],1'b0,c15,   c16,ALUOut[15],set);
   nor nor1(Zero, ALUOut[0],ALUOut[1],ALUOut[2],ALUOut[3],ALUOut[4],ALUOut[5],ALUOut[6],
   ALUOut[7],ALUOut[8],ALUOut[9],ALUOut[10],ALUOut[11],ALUOut[12],ALUOut[13], ALUOut[14],ALUOut[15]);
endmodule

// 4-bit multiplexer for choice of operation
module MUX4x1(out, a, b, c, d, op);

    output out;
    input a, b, c, d;
    input [1:0] op;

    not (nop0, op[0]), (nop1, op[1]);
    and (T1, a, nop1, nop0), (T2, b, nop1, op[0]),(T3, c, op[1], nop0), (T4, d, op[1], op[0]);
    or (out, T1, T2, T3, T4);
endmodule

//2-bit multiplexer for a-invert and b-invert
module MUX2x1(Y, D0, D1, op); 

output Y;
input D0, D1, op;
wire T1, T2, nOp;

and (T1, D1, op), (T2, D0, nOp);
not (nOp, op);
or (Y, T1, T2);

endmodule

//code for the multiplexer that selects sign extend (D1), or read data 2 (D0)
module _16bitMUX2x1(Y, D0, D1, S);

input [15:0] D0, D1;
input S;
output [15:0] Y;

MUX2x1 mux0(Y[0],D0[0], D1[0], S);
MUX2x1 mux1(Y[1],D0[1], D1[1], S);
MUX2x1 mux2(Y[2],D0[2], D1[2], S);
MUX2x1 mux3(Y[3],D0[3], D1[3], S);
MUX2x1 mux4(Y[4],D0[4], D1[4], S);
MUX2x1 mux5(Y[5],D0[5], D1[5], S);
MUX2x1 mux6(Y[6],D0[6], D1[6], S);
MUX2x1 mux7(Y[7],D0[7], D1[7], S);
MUX2x1 mux8(Y[8],D0[8], D1[8], S);
MUX2x1 mux9(Y[9],D0[9], D1[9], S);
MUX2x1 mux10(Y[10],D0[10], D1[10], S);
MUX2x1 mux11(Y[11],D0[11], D1[11], S);
MUX2x1 mux12(Y[12],D0[12], D1[12], S);
MUX2x1 mux13(Y[13],D0[13], D1[13], S);
MUX2x1 mux14(Y[14],D0[14], D1[14], S);
MUX2x1 mux15(Y[15],D0[15], D1[15], S);
endmodule

//code for multiplexer that is used to select register using bits [7:6] (D1), or [9:8] (D0)
module _2bitMUX2x1(Y, D0, D1, S);

input [1:0] D0, D1;
input S;
output [1:0] Y;

MUX2x1 mux0(Y[0],D0[0], D1[0], S);
MUX2x1 mux1(Y[1],D0[1], D1[1], S);

endmodule

// half adder
module HalfAdder(sum, carry, x, y);
    input x, y;
    output sum, carry;

    xor xGate1(sum, x, y); //logic for sum
    and aGate1(carry, x, y); // logic for carry

endmodule

// full adder
module FullAdder(sum, carry, x, y, carryIn);
    input x, y, carryIn;
    output sum, carry;

    HalfAdder Ha1(sum1, carry1, x, y), Ha2(sum, carry2, sum1, carryIn);
    or Or1(carry,carry1,carry2);
endmodule


// 1-bit ALU for bits 0-2
module ALU1 (a,b,ainvert,binvert,op,less,carryin,carryout,result);
   input a,b,less,carryin,ainvert,binvert;
   input [1:0] op;
   output carryout,result;
   
   not negateA(aNot,a);
   not negateB(bNot,b);
   MUX2x1 aInvert(aMuxOutput,a,aNot,ainvert);
   MUX2x1 bInvert(bMuxOutput,b,bNot,binvert);
   and and1(andOutput,aMuxOutput,bMuxOutput);
   or or1(orOutput,aMuxOutput,bMuxOutput);
   FullAdder fa1(sum,carryout,aMuxOutput,bMuxOutput,carryin);
   MUX4x1 opMUX(result,andOutput,orOutput,sum,less,op[1:0]);
  
endmodule

// 1-bit ALU for the most significant bit
module ALUmsb (a,b,ainvert,binvert,op,less,carryin,carryout,result,sum);
   input a,b,less,carryin,ainvert,binvert;
   input [1:0] op;
   output carryout,result,sum;
   
   not negateA(aNot,a);
   not negateB(bNot,b);
   MUX2x1 aInvert(aMuxOutput,a,aNot,ainvert);
   MUX2x1 bInvert(bMuxOutput,b,bNot,binvert);
   and and1(andOutput,aMuxOutput,bMuxOutput);
   or or1(orOutput,aMuxOutput,bMuxOutput);
   FullAdder fa1(sum,carryout,aMuxOutput,bMuxOutput,carryin);
   MUX4x1 opMUX(result,andOutput,orOutput,sum,less,op[1:0]);
endmodule

module MainControl (Op,Control); 
  input [3:0] Op;
  output reg [6:0] Control;
// Control bits: RegDst,ALUSrc,RegWrite,ALUOp
  always @(Op) case (Op)
  //what this code does is it detects the upper 4 bits of the code and depending on the input
    // for example, "0000" the control will be assigned a 7 bit binary code corresponding to the RegDst,ALUSrc,RegWrite,and ALUOp
    //for example,  4'b0000: Control <= 7'b1010010;
    4'b0000: Control <= 7'b1010010; // add
    4'b0001: Control <= 7'b1010110; // sub
    4'b0010: Control <= 7'b1010000; // and
    4'b0011: Control <= 7'b1010001; // or
    4'b0100: Control <= 7'b1011100; // nor
    4'b0101: Control <= 7'b1011101; // nand
    4'b0110: Control <= 7'b1010111; // slt
    4'b0111: Control <= 7'b0110010; // ADDI
  endcase
endmodule

/*module ALUControl (ALUOp,FuncCode,ALUCtl); 
  input [1:0] ALUOp;
  input [5:0] FuncCode;
  output reg [3:0] ALUCtl;
  always @(ALUOp,FuncCode) case (ALUOp)
    2'b00: ALUCtl <= 4'b0010; // add
    2'b01: ALUCtl <= 4'b0110; // subtract
    2'b10: case (FuncCode)
	     32: ALUCtl <= 4'b0010; // add
	     34: ALUCtl <= 4'b0110; // sub
	     36: ALUCtl <= 4'b0000; // and
	     37: ALUCtl <= 4'b0001; // or
	     39: ALUCtl <= 4'b1100; // nor
	     42: ALUCtl <= 4'b0111; // slt
    endcase
  endcase
endmodule*/ //not needed

module CPU (clock,PC,ALUOut,IR);
  input clock;
  output [15:0] ALUOut,IR,PC;
  reg[15:0] PC;
  reg[15:0] IMemory[0:1023];
  wire [15:0] IR,NextPC,A,B,ALUOut,RD2,SignExtend;
  wire [3:0] ALUctl;
  //wire [1:0] ALUOp; we don't need, we get the control code directly from the main control
  wire [1:0] WR; 
// Test Program
  initial begin 
    IMemory[0] = 16'b0111_00_01_00001111;  // addi $1, $0,  15   ($1=15)
    IMemory[1] = 16'b0111_00_10_00000111;  // addi $2, $0,  7    ($2=7)
    IMemory[2] = 16'b0010_01_10_11_000000;  // and  $3, $1, $2  ($3=7)
    IMemory[3] = 16'b0001_01_11_10_000000;  // sub  $2, $1, $3  ($2=8)
    IMemory[4] = 16'b0011_10_11_10_000000;  // or   $2, $2, $3  ($2=15)
    IMemory[5] = 16'b0000_10_11_11_000000;  // add  $3, $2, $3  ($3=22)
    IMemory[6] = 16'b0100_10_11_01_000000;  // nor  $1, $2, $3  ($1=-32)
    IMemory[7] = 16'b0110_11_10_01_000000;  // slt  $1, $3, $2  ($1=0)
    IMemory[8] = 16'b0110_10_11_01_000000;  // slt  $1, $2, $3  ($1=1)
  end
  initial PC = 0;
  assign IR = IMemory[PC>>1];
  _2bitMUX2x1 _2bitMUX(WR,IR[9:8],IR[7:6],RegDst);// RegDst Mux
  //assign WR = (RegDst) ? IR[7:6]: IR[9:8]; // behavioral RegDst Mux 
  _16bitMUX2x1 _16bitMUX(B,RD2,SignExtend,ALUSrc); // ALUSrc Mux
  //assign B  = (ALUSrc) ? SignExtend: RD2; // behavioral ALUSrc Mux //replace with multiplexers
  assign SignExtend = {{8{IR[7]}},IR[7:0]}; // sign extension unit
  reg_file rf (IR[11:10],IR[9:8],WR,ALUOut,RegWrite,A,RD2,clock);
  alu fetch (4'b0010,PC,2,NextPC,Unused);
  alu ex (ALUctl, A, B, ALUOut, Zero);
  MainControl MainCtr (IR[15:12],{RegDst,ALUSrc,RegWrite,ALUctl}); 
  // ALUControl ALUCtrl(ALUOp, IR[5:0], ALUctl); // ALU control unit not needed
  always @(negedge clock) begin 
    PC <= NextPC;
  end
endmodule

// Test module
module test ();
  reg clock;
  wire signed [15:0] WD,IR,PC;
  CPU test_cpu(clock,PC,WD,IR);
  always #1 clock = ~clock;
  initial begin
    $display ("Clock PC   IR                                 WD");
    $monitor ("%b     %2d   %b  %3d (%b)",clock,PC,IR,WD,WD);
    clock = 1;
    #16 $finish;
  end
endmodule

/* Output
Clock PC   IR                                 WD
1      0   00100000000010010000000000001111   15 (00000000000000000000000000001111)
0      4   00100000000010100000000000000111    7 (00000000000000000000000000000111)
1      4   00100000000010100000000000000111    7 (00000000000000000000000000000111)
0      8   00000001001010100101100000100100    7 (00000000000000000000000000000111)
1      8   00000001001010100101100000100100    7 (00000000000000000000000000000111)
0     12   00000001001010110101000000100010    8 (00000000000000000000000000001000)
1     12   00000001001010110101000000100010    8 (00000000000000000000000000001000)
0     16   00000001010010110101000000100101   15 (00000000000000000000000000001111)
1     16   00000001010010110101000000100101   15 (00000000000000000000000000001111)
0     20   00000001010010110101100000100000   22 (00000000000000000000000000010110)
1     20   00000001010010110101100000100000   22 (00000000000000000000000000010110)
0     24   00000001010010110100100000100111  -32 (11111111111111111111111111100000)
1     24   00000001010010110100100000100111  -32 (11111111111111111111111111100000)
0     28   00000001011010100100100000101010    0 (00000000000000000000000000000000)
1     28   00000001011010100100100000101010    0 (00000000000000000000000000000000)
0     32   00000001010010110100100000101010    1 (00000000000000000000000000000001)
1     32   00000001010010110100100000101010    1 (00000000000000000000000000000001)
*/