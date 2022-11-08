// Behavioral model of MIPS, 3-stage pipeline for R-types and addi only
// Implements forwarding

//replace reg file with project 1
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

// 1-bit ALU for bits 0-14
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
    4'b0000: Control <= 7'b1010010; // add
    // ... add all the other control codes for the other operations
    //what this code does is it detects the upper 4 bits of the code and depending on the input
    // for example, "0000" the control will be assigned a 7 bit binary code corresponding to the RegDst,ALUSrc,RegWrite,and ALUOp
    //for example,  4'b0000: Control <= 7'b1010010;
    4'b0001: Control <= 7'b1010110; // sub
    4'b0010: Control <= 7'b1010000; // and
    4'b0011: Control <= 7'b1010001; // or
    4'b0100: Control <= 7'b1011100; // nor
    4'b0101: Control <= 7'b1011101; // nand
    4'b0110: Control <= 7'b1010111; // slt
    4'b0111: Control <= 7'b0110010; // ADDI
  endcase
endmodule


module CPU (clock,PC,IFID_IR,IDEX_IR,WD);
  input clock;
  output [15:0] PC,IFID_IR,IDEX_IR,WD;
  
  /*initial begin
// Program with nop's - no hazards
    IMemory[0]  = 16'b0111_00_01_00001111;  // addi $1, $0,  15   ($t1=15)
    IMemory[1]  = 16'b0111_00_10_00000111;  // addi $2, $0,  7    ($t2=7)
    IMemory[2]  = 16'b00000000;  // nop
    IMemory[3]  = 16'b0010_01_10_11_000000;  // and  $3, $1, $2  ($t3=7)
    IMemory[4]  = 16'b00000000;  // nop
    IMemory[5]  = 16'b0001_01_11_10_000000;  // sub  $2, $1, $3  ($t2=8)
    IMemory[6]  = 16'b00000000;  // nop
    IMemory[7]  = 16'b0011_10_11_10_000000;  // or   $2, $2, $3  ($t2=15)
    IMemory[8]  = 16'b00000000;  // nop
    IMemory[9]  = 16'b0000_10_11_11_000000;  // add  $3, $2, $3  ($t3=22)
    IMemory[10] = 16'b00000000;  // nop
    IMemory[11] = 16'b0100_10_11_01_000000;  // nor  $1, $2, $3  ($t1=-32)
    IMemory[12] = 16'b0110_11_10_01_000000;  // slt  $1, $3, $2  ($t1=0)
    IMemory[13] = 16'b0110_10_11_01_000000;  // slt  $1, $2, $3  ($t1=1)
  end
  */
  
  initial begin 
// Program without nop's - wrong results due to data hazards
    IMemory[0]  = 16'b0111_00_01_00001111;  // addi $1, $0,  15   ($t1=15)
    IMemory[1]  = 16'b0111_00_10_00000111;  // addi $2, $0,  7    ($t2=7)
    IMemory[2]  = 16'b0010_01_10_11_000000;  // and  $3, $1, $2  ($t3=7)
    IMemory[3]  = 16'b0001_01_11_10_000000;  // sub  $2, $1, $3  ($t2=8)
    IMemory[4]  = 16'b0011_10_11_10_000000;  // or   $2, $2, $3  ($t2=15)
    IMemory[5]  = 16'b0000_10_11_11_000000;  // add  $3, $2, $3  ($t3=22)
    IMemory[6] = 16'b0100_10_11_01_000000;  // nor  $1, $2, $3  ($t1=-32)
    IMemory[7] = 16'b0110_11_10_01_000000;  // slt  $1, $3, $2  ($t1=0)
    IMemory[8] = 16'b0110_10_11_01_000000;  // slt  $1, $2, $3  ($t1=1)
  end

// Pipeline stages
//=== IF STAGE ===
   wire [15:0] NextPC;
   reg[15:0] PC, IMemory[0:1023];
//--------------------------------
   reg[15:0] IFID_IR;
//--------------------------------
   alu fetch (4'b0010,PC,2,NextPC,Unused);

//=== ID STAGE ===
   wire [6:0] Control;
   wire [15:0] RD1,RD2,SignExtend,WD;
   wire [15:0] FWD_RD1,FWD_RD2; // Outputs of the forwarding muxes
   reg [15:0] IDEX_IR; // For monitoring the pipeline
   reg IDEX_RegWrite,IDEX_ALUSrc,IDEX_RegDst;
   reg [3:0]  IDEX_ALUctl;
   reg [15:0] IDEX_RD1,IDEX_RD2,IDEX_SignExt;
   reg [1:0]  IDEX_rt,IDEX_rd;
   wire [1:0] WR;
   reg_file rf (IFID_IR[11:10],IFID_IR[9:8],WR,WD,IDEX_RegWrite,RD1,RD2,clock);
   MainControl MainCtr (IFID_IR[15:12],Control); 
   assign SignExtend = {{8{IFID_IR[7]}},IFID_IR[7:0]}; 

//=== EXE STAGE ===
   wire [15:0] B,ALUOut;
   wire [2:0] ALUctl;
   alu ex (IDEX_ALUctl, IDEX_RD1, B, ALUOut, Zero);
  // ALUControl ALUCtrl(IDEX_ALUOp, IDEX_SignExt[5:0], ALUctl); // ALU control unit
   assign B  = (IDEX_ALUSrc) ? IDEX_SignExt: IDEX_RD2;   // ALUSrc Mux 
   assign WR = (IDEX_RegDst) ? IDEX_rd: IDEX_rt;         // RegDst Mux
   assign WD = ALUOut;
   
   //change these bit fields in order to implement fowarding
   //implement these multiplexers from gate level
// Forwarding multiplexers
   assign FWD_RD1 = (IDEX_RegWrite && WR==IFID_IR[11:10]) ? ALUOut: RD1;
   assign FWD_RD2 = (IDEX_RegWrite && WR==IFID_IR[9:8]) ? ALUOut: RD2;

   initial begin
    PC = 0;
    IFID_IR = 0; // clear pipeline register to avoid forwarding from empty pipeline
    IDEX_RegWrite = 0; 
   end

// Running the pipeline
   always @(negedge clock) begin

// Stage 1 - IF
    PC <= NextPC;
    IFID_IR <= IMemory[PC>>1];

// Stage 2 - ID
    IDEX_IR <= IFID_IR; // For monitoring the pipeline
    {IDEX_RegDst,IDEX_ALUSrc,IDEX_RegWrite,IDEX_ALUctl} <= Control;    

//  No Forwarding
    IDEX_RD1 <= RD1; 
    IDEX_RD2 <= RD2;

//  Forwarding
IDEX_RD1 <= FWD_RD1; 
IDEX_RD2 <= FWD_RD2;

    IDEX_SignExt <= SignExtend;
    IDEX_rt <= IFID_IR[9:8];
    IDEX_rd <= IFID_IR[7:6];

// Stage 3 - EX
// No transfers needed here - on negedge WD is written into register WR
  end
endmodule

// Test module
module test ();
  reg clock;
  wire signed [15:0] PC,IFID_IR,IDEX_IR,WD;
  CPU test_cpu(clock,PC,IFID_IR,IDEX_IR,WD);
  always #1 clock = ~clock;
  initial begin
    $display ("PC  IFID_IR   IDEX_IR   WD");
    $monitor ("%2d  %h  %h  %2d",PC,IFID_IR,IDEX_IR,WD);
    clock = 1;
    #29 $finish;
  end
endmodule

/* Output
Program with nop's
---------------------------
 PC  IFID_IR   IDEX_IR   WD
 0  00000000  xxxxxxxx   x
 4  2009000f  00000000   x
 8  200a0007  2009000f  15
12  00000000  200a0007   7
16  012a5824  00000000   0
20  00000000  012a5824   7
24  012b5022  00000000   0
28  00000000  012b5022   8
32  014b5025  00000000   0
36  00000000  014b5025  15
40  014b5820  00000000   0
44  00000000  014b5820  22
48  014b4827  00000000   0
52  016a482a  014b4827  -32
56  014b482a  016a482a   0
60  xxxxxxxx  014b482a   1

Program without nop's
--------------------------
PC  IFID_IR   IDEX_IR   WD
 0  00000000  xxxxxxxx   x
 4  2009000f  00000000   x
 8  200a0007  2009000f  15
12  012a5824  200a0007   7
16  012b5022  012a5824   X
20  014b5025  012b5022   x
24  014b5820  014b5025   X
28  014b4827  014b5820   x
32  016a482a  014b4827   X
36  014b482a  016a482a   X
40  xxxxxxxx  014b482a   X
44  xxxxxxxx  xxxxxxxx   X
48  xxxxxxxx  xxxxxxxx   X
52  xxxxxxxx  xxxxxxxx   X
56  xxxxxxxx  xxxxxxxx   X
60  xxxxxxxx  xxxxxxxx   X
*/
