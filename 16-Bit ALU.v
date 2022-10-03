// 4-bit MIPS ALU in Verilog (1-bit ALU bihavioral implementation)

module ALU (op,a,b,result,zero);
   input [15:0] a;
   input [15:0] b;
   input [3:0] op;
   output [15:0] result;
   output zero;
   //module ALU1 (a,b,ainvert,binvert,op,less,carryin,carryout,result);
   ALU1   alu0 (a[0],b[0],op[3],op[2],op[1:0],set, op[2],c1,result[0]);
   ALU1   alu1 (a[1],b[1],op[3],op[2],op[1:0],1'b0,c1,   c2,result[1]);
   ALU1   alu2 (a[2],b[2],op[3],op[2],op[1:0],1'b0,c2,   c3,result[2]);
   ALU1   alu3 (a[3],b[3],op[3],op[2],op[1:0],1'b0,c3,   c4,result[3]);
   ALU1   alu4 (a[4],b[4],op[3],op[2],op[1:0],1'b0,c4,   c5,result[4]);
   ALU1   alu5 (a[5],b[5],op[3],op[2],op[1:0],1'b0,c5,   c6,result[5]);
   ALU1   alu6 (a[6],b[6],op[3],op[2],op[1:0],1'b0,c6,   c7,result[6]);
   ALU1   alu7 (a[7],b[7],op[3],op[2],op[1:0],1'b0,c7,   c8,result[7]);
   ALU1   alu8 (a[8],b[8],op[3],op[2],op[1:0],1'b0,c8,   c9,result[8]);
   ALU1   alu9 (a[9],b[9],op[3],op[2],op[1:0],1'b0,c9,   c10,result[9]);
   ALU1   alu10 (a[10],b[10],op[3],op[2],op[1:0],1'b0,c10,   c11,result[10]);
   ALU1   alu11 (a[11],b[11],op[3],op[2],op[1:0],1'b0,c11,   c12,result[11]);
   ALU1   alu12 (a[12],b[12],op[3],op[2],op[1:0],1'b0,c12,   c13,result[12]);
   ALU1   alu13 (a[13],b[13],op[3],op[2],op[1:0],1'b0,c13,   c14,result[13]);
   ALU1   alu14 (a[14],b[14],op[3],op[2],op[1:0],1'b0,c14,   c15,result[14]);
   ALUmsb alu15 (a[15],b[15],op[3],op[2],op[1:0],1'b0,c15,   c16,result[15],set);
   nor nor1(zero, result[0],result[1],result[2],result[3]);
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
  
   //change this part
   /*assign a_inv = ~a;	
   assign a1 = ainvert? a_inv: a;
   assign b_inv = ~b;	
   assign b1 = binvert? b_inv: b; 
   assign a_and_b = a1 && b1;
   assign a_or_b = a1 || b1;
   assign {carryout,sum} = a + b1 + carryin;
   always @ (a_and_b,a_or_b,sum,less,op) 
       case (op) 
          2'b00: result = a_and_b;
          2'b01: result = a_or_b;
          2'b10: result = sum;
          2'b11: result = less;
       endcase
       //end change here*/
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
   /*//change code below
   assign a_inv = ~a;
   assign a1 = ainvert? a_inv: a; 
   assign b_inv = ~b;
   assign b1 = binvert? b_inv: b; 
   assign a_and_b = a1 && b1;
   assign a_or_b = a1 || b1;
   assign {carryout,sum} = a + b1 + carryin;
   always @ (a_and_b,a_or_b,sum,less,op) 
       case (op) 
          2'b00: result = a_and_b;
          2'b01: result = a_or_b;
          2'b10: result = sum;
          2'b11: result = less;
       endcase 
       //end change here*/
endmodule

// Test Module 
module testALU;
   reg signed [15:0] a;
   reg signed [15:0] b;
   reg [3:0] op;
   wire signed [15:0] result;
   wire zero;
   ALU alu (op,a,b,result,zero);
   initial begin
    $display("op        a                       b                           result              zero");
    $monitor ("%b %b  (%d) %b (%d) %b (%d) %b",op,a,a,b,b,result,result,zero);
       op = 4'b0000; a = 16'b0000000000000111; b = 16'b0000000000000001;  // AND
	#1 op = 4'b0001; a = 16'b0000000000000101; b = 16'b0000000000000010;  // OR
	#1 op = 4'b0010; a = 16'b0000000000000100; b = 16'b0000000000000010;  // ADD
	#1 op = 4'b0010; a = 16'b0110001000000111; b = 16'b0100000100000001;  // ADD
	#1 op = 4'b0110; a = 16'b0000000000100101; b = 16'b0000000000100011;  // SUB
	#1 op = 4'b0110; a = 16'b1000000000001111; b = 16'b0000000000000001;  // SUB
	#1 op = 4'b0111; a = 16'b0000000000000101; b = 16'b0000000000000001;  // SLT
	#1 op = 4'b0111; a = 16'b0000000000001110; b = 16'b0000000000001111;  // SLT
	#1 op = 4'b1100; a = 16'b0000000000000101; b = 16'b0000000000000010;  // NOR
	#1 op = 4'b1101; a = 16'b0000000000000101; b = 16'b0000000000000010;  // NAND
   end
endmodule

/* Output
op   a        b        result   zero
0000 0111( 7) 0001( 1) 0001( 1) 0
0001 0101( 5) 0010( 2) 0111( 7) 0
0010 0100( 4) 0010( 2) 0110( 6) 0
0010 0111( 7) 0001( 1) 1000(-8) 0
0110 0101( 5) 0011( 3) 0010( 2) 0
0110 1111(-1) 0001( 1) 1110(-2) 0
0111 0101( 5) 0001( 1) 0000( 0) 1
0111 1110(-2) 1111(-1) 0001( 1) 0
1100 0101( 5) 0010( 2) 1000(-8) 0
1101 0101( 5) 0010( 2) 1111(-1) 0
*/