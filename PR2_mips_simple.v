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

module MainControl (Op,Control); 
  input [3:0] Op;
  output reg [10:0] Control;
// RegDst,ALUSrc,MemtoReg,RegWrite,MemWrite,Beq,Bne,ALUctl
  always @(Op) case (Op)
    //4'b0000: Control <= 11'b10010_00_0000; // Rtype
    4'b0000: Control <= 11'b10010_00_0010; // ADD
    4'b0001: Control <= 11'b10010_00_0110; // SUB
    4'b0010: Control <= 11'b10010_00_0000; // AND
    4'b0011: Control <= 11'b10010_00_0001; // OR
    4'b0100: Control <= 11'b10010_00_1100; // NOR
    4'b0101: Control <= 11'b10010_00_1101; // NAND
    4'b0110: Control <= 11'b10010_00_0111; // SLT
    4'b0111: Control <= 11'b01010_00_0010; // ADDI
    4'b1000: Control <= 11'b01110_00_0010; // LW    
    4'b1001: Control <= 11'b01001_00_0010; // SW    
    4'b1010: Control <= 11'b00000_10_0110; // BEQ
    4'b1011: Control <= 11'b00000_01_0110; // BNE
    
    
  endcase
endmodule

//branch control gate-level
module BranchControl(Zero, Beq, Bne, Out);
  input Zero, Beq, Bne;
  output Out;
  wire BeqAndZero, notZero;
  not invertZero(Zero, notZero);
  and and1(Beq, Zero, BeqAndZero), and2(Bne, notZero, BneAndNotZero);
  or BranchControlOut(BeqAndZero,BneAndNotZero,Out);
  

module ALUControl (ALUOp,FuncCode,ALUCtl); 
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
endmodule

module CPU (clock,WD,IR,PC);

  input clock;
  output [15:0] WD,IR,PC;
  reg[15:0] PC, IMemory[0:1023], DMemory[0:1023];
  wire [15:0] IR,SignExtend,NextPC,RD2,A,B,ALUOut,PCplus4,Target;
  wire [1:0] WR;
  wire [3:0] ALUctl;
  
  initial begin 
 // Program: swap memory cells and compute absolute value
    IMemory[0] = 16'b1000_00_01_00000000;  // lw $1, 0($0) 
    IMemory[1] = 16'b1000_00_10_00000010;  // lw $2, 2($0)
    IMemory[2] = 16'b0110_01_10_11_000000;  // slt $3, $1, $2
    IMemory[3] = 16'b1010_11_00_00000010;  // beq $3, $0, IMemory[6] 
    IMemory[4] = 16'b1001_00_01_00000010;  // sw $1, 2($0) 
    IMemory[5] = 16'b1001_00_10_00000000;  // sw $2, 0($0) 
    IMemory[6] = 16'b1000_00_01_00000000;  // lw $1, 0($0) 
    IMemory[7] = 16'b1000_00_10_00000010;  // lw $2, 2($0)
    IMemory[8] = 16'b0100_10_10_10_000000;  // nor $2, $2, $2 (sub $3, $1, $2 in two's complement)
    IMemory[9] = 16'b0111_10_10_00000001;  // addi $2, $2, 1 
    IMemory[10] = 16'b0000_01_10_11_000000;  // add $3, $1, $2 
    
 // Data
    DMemory [0] = 16'd5; // swap the cells and see how the simulation output changes
    DMemory [1] = 16'd7;
  end
  initial PC = 0;
  assign IR = IMemory[PC>>1];
  assign SignExtend = {{8{IR[7]}},IR[7:0]}; // sign extension
  reg_file rf (IR[11:10],IR[9:8],WR,WD,RegWrite,A,RD2,clock);
  alu fetch (4'b0010,PC,2,PCplus4,Unused1);
  alu ex (ALUctl, A, B, ALUOut, Zero);
  alu branch (4'b0010,SignExtend<<1,PCplus4,Target,Unused2);
  MainControl MainCtr (IR[15:12],{RegDst,ALUSrc,MemtoReg,RegWrite,MemWrite,Beq,Bne,ALUctl}); 
  _2bitMUX2x1 _2bitMUX(WR,IR[9:8],IR[7:6],RegDst);// RegDst Mux
  //assign WR = (RegDst) ? IR[7:6]: IR[9:8]; // Behavioral RegDst Mux
  _16bitMUX2x1 _16bitMUX1(WD,ALUOut,DMemory[ALUOut>>1],MemtoReg); // MemtoReg Mux gate-level
  //assign WD = (MemtoReg) ? DMemory[ALUOut>>1]: ALUOut; // Behavioral MemtoReg Mux
   _16bitMUX2x1 _16bitMUX2(B,RD2,SignExtend,ALUSrc); // ALUSrc Mux
  //assign B  = (ALUSrc) ? SignExtend: RD2; // Behavioral ALUSrc Mux 
  BranchControl BranchControl(Zero, Beq, Bne, Out); //branch control gate-level
  _16bitMUX2x1 _16bitMUX3(NextPC,PCplus4,Target,Out);
  //assign NextPC = (Beq && Zero || Bne && ~Zero) ? Target: PCplus4; // Behavioral Branch Mux
  always @(negedge clock) begin 
    PC <= NextPC;
    if (MemWrite) DMemory[ALUOut>>1] <= RD2;
  end
endmodule

// Test module
module test ();
  reg clock;
  wire signed [15:0] WD,IR,PC;
  CPU test_cpu(clock,WD,IR,PC);
  always #1 clock = ~clock;
  initial begin
    $display ("PC  IR                                WD");
    $monitor ("%2d  %b %2d (%b)",PC,IR,WD,WD);
    clock = 1;
    #20 $finish;
  end
endmodule

/* Output
PC  IR                                WD
 0  10001100000010010000000000000000  5 (00000000000000000000000000000101)
 4  10001100000010100000000000000100  7 (00000000000000000000000000000111)
 8  00000001001010100101100000101010  1 (00000000000000000000000000000001)
12  00010001011000000000000000000010  1 (00000000000000000000000000000001)
16  10101100000010010000000000000100  4 (00000000000000000000000000000100)
20  10101100000010100000000000000000  0 (00000000000000000000000000000000)
24  10001100000010010000000000000000  7 (00000000000000000000000000000111)
28  10001100000010100000000000000100  5 (00000000000000000000000000000101)
32  00000001010010100101000000100111 -6 (11111111111111111111111111111010)
36  00100001010010100000000000000001 -5 (11111111111111111111111111111011)
40  00000001001010100101100000100000  2 (00000000000000000000000000000010)
*/