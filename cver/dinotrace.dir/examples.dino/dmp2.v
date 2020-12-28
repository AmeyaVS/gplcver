module GCD_sim;
 reg Reset;
 reg [7:0] X;
 reg [7:0] Y;
 wire [7:0] gcd_output;
 GCD GCD_1(X, Y, Reset, gcd_output);
 initial
  begin
   #8
    $dumpfile("xx.dmp");
    $dumpvars(0, GCD_1.compare_var, GCD_1.gcd_output);
  //$dumpvars(1, GCD_sim, GCD_1, GCD_1.compare_var);
 //  $dumplimit(10000);
  // $dumpvars;
   #1;
   // $dumpall;
   // $dumpon;
   // $dumpoff;
   
  end
 initial 
  begin
    //$dumpfile("xx.dmp");
   // $dumpvars(1, GCD_sim, GCD_1.COMPARE, GCD_1.compare_var);
//    $dumpvars(0, GCD_1.compare_var);

   // repeat (5) 
   repeat (50) 
    begin
     #10 ;
     X = 8'b1;
     Y = 8'b1;
     Reset = 1'b0;
     #10 ;
     if (gcd_output !== 8'b1) $display($stime, , 
      "gcd_output(%b) !== 8'b00000001", gcd_output);
     X = 8'b10;
     Y = 8'b1;
     Reset = 1'b0;
     #10 ;
     if (gcd_output !== 8'b1) $display($stime, , 
      "gcd_output(%b) !== 8'b00000001", gcd_output);
     // $finish(2);
     X = 8'b110;
     Y = 8'b11;
     Reset = 1'b0;
     #10 ;
     if (gcd_output !== 8'b11) $display($stime, , 
      "gcd_output(%b) !== 8'b00000011", gcd_output);
     X = 8'b1100;
     Y = 8'b11;
     Reset = 1'b0;
     #10 ;
     if (gcd_output !== 8'b11) $display($stime, , 
      "gcd_output(%b) !== 8'b00000011", gcd_output);
     X = 8'b1;
     Y = 8'b10;
     Reset = 1'b0;
     #10 ;
     if (gcd_output !== 8'b1) $display($stime, , 
      "gcd_output(%b) !== 8'b00000001", gcd_output);
     X = 8'b11;
     Y = 8'b110;
     Reset = 1'b0;
     #10 ;
     if (gcd_output !== 8'b11) $display($stime, , 
      "gcd_output(%b) !== 8'b00000011", gcd_output);
     X = 8'b11;
     Y = 8'b1100;
     Reset = 1'b0;
     #10 ;
     if (gcd_output !== 8'b11) $display($stime, , 
      "gcd_output(%b) !== 8'b00000011", gcd_output);
     X = 8'b1100;
     Y = 8'b110;
     Reset = 1'b1;
     #10 ;
     if (gcd_output !== 8'b0) $display($stime, , 
      "gcd_output(%b) !== 8'b00000000", gcd_output);
     X = 8'b11100;
     Y = 8'b101;
     Reset = 1'b0;
     #10 ;
     if (gcd_output !== 8'b1) $display($stime, , 
      "gcd_output(%b) !== 8'b00000001", gcd_output);
     X = 8'b101;
     Y = 8'b11100;
     Reset = 1'b0;
     #10 ;
     if (gcd_output !== 8'b1) $display($stime, , 
      "gcd_output(%b) !== 8'b00000001", gcd_output);
     X = 8'b11100;
     Y = 8'b111;
     Reset = 1'b1;
     #10 ;
     if (gcd_output !== 8'b0) $display($stime, , 
      "gcd_output(%b) !== 8'b00000000", gcd_output);
     X = 8'b11100;
     Y = 8'b11100;
     Reset = 1'b0;
     #10 ;
     if (gcd_output !== 8'b11100) $display($stime, , 
      "gcd_output(%b) !== 8'b00011100", gcd_output);
     X = 8'b11100;
     Y = 8'b11100;
     Reset = 1'b1;
     #10 ;
     if (gcd_output !== 8'b0) $display($stime, , 
      "gcd_output(%b) !== 8'b00000000", gcd_output);
     X = 8'b11;
     Y = 8'b11000000;
     Reset = 1'b0;
     #10 ;
     if (gcd_output !== 8'b11) $display($stime, , 
      "gcd_output(%b) !== 8'b00000011", gcd_output);
     X = 8'b11;
     Y = 8'b11000000;
     Reset = 1'b0;
     #10 ;
     if (gcd_output !== 8'b11) $display($stime, , 
      "gcd_output(%b) !== 8'b00000011", gcd_output);
     X = 8'b1000000;
     Y = 8'b11010000;
     Reset = 1'b0;
     #10 ;
     if (gcd_output !== 8'b10000) $display($stime, , 
      "gcd_output(%b) !== 8'b00010000", gcd_output);
     X = 8'b100110;
     Y = 8'b10011110;
     Reset = 1'b0;
     #10 ;
     if (gcd_output !== 8'b10) $display($stime, , 
      "gcd_output(%b) !== 8'b00000010", gcd_output);
     X = 8'b10011110;
     Y = 8'b100110;
     Reset = 1'b0;
     #10 ;
     if (gcd_output !== 8'b10) $display($stime, , 
      "gcd_output(%b) !== 8'b00000010", gcd_output);
     X = 8'b10011110;
     Y = 8'b11000000;
     Reset = 1'b0;
     #10 ;
     if (gcd_output !== 8'b10) $display($stime, , 
      "gcd_output(%b) !== 8'b00000010", gcd_output);
     X = 8'b10011110;
     Y = 8'b11000000;
     Reset = 1'b0;
     #10 ;
     if (gcd_output !== 8'b10) $display($stime, , 
      "gcd_output(%b) !== 8'b00000010", gcd_output);
     X = 8'b0;
     Y = 8'b11000000;
     Reset = 1'b0;
     #10 ;
     if (gcd_output !== 8'b0) $display($stime, , 
      "gcd_output(%b) !== 8'b00000000", gcd_output);
     X = 8'b11000000;
     Y = 8'b0;
     Reset = 1'b0;
     #10 ;
     if (gcd_output !== 8'b0) $display($stime, , 
      "gcd_output(%b) !== 8'b00000000", gcd_output);
     X = 8'b0;
     Y = 8'b0;
     Reset = 1'b0;
     #10 ;
     if (gcd_output !== 8'b0) $display($stime, , 
      "gcd_output !== 8'b00000000");
     X = 8'b0;
     Y = 8'b0;
     Reset = 1'b0;
     #10 ;
     if (gcd_output !== 8'b0) $display($stime, , 
      "gcd_output(%b) !== 8'b00000000", gcd_output);
     // dump all variables each time through loop
     $dumpall;
     $dumpflush;
    end
   $finish(3);
  end

endmodule

module GCD(X, Y, Reset, gcd_output);
 input X;
 input Y;
 input Reset;
 output gcd_output;
 wire [7:0] X;
 wire [7:0] Y;
 reg [1:0] compare_var;
 reg [7:0] gcd_output;
 reg resetvar;
 reg [7:0] xvar;
 reg [7:0] yvar;

function [1:0] COMPARE;
 input[7:0] x1;
 input[7:0] x2;
 integer i;
  begin : flag
   i = 7;
   while (i >= 0) 
    begin
     if ((x1[i] == 1'b1) && (x2[i] == 1'b0)) 
      begin
       COMPARE = 2'b10;
       disable flag;
      end
     else if ((x1[i] == 1'b0) && (x2[i] == 1'b1)) 
      begin
       COMPARE = 2'b1;
       disable flag;
      end
     i = i - 1;
    end
   COMPARE = 2'b11;
  end
endfunction

 always @(X or Y or Reset) 
  begin
   xvar = X;
   yvar = Y;
   resetvar = Reset;
   if (xvar == 8'b0) gcd_output = 8'b0;
   if (yvar == 8'b0) gcd_output = 8'b0;
   if (((resetvar == 1'b0) && (xvar !== 8'b0)) && (yvar !== 8'b0)) 
    begin
     compare_var = COMPARE(xvar, yvar);
     while (compare_var !== 2'b11) 
      begin
       if (compare_var == 2'b1) yvar = yvar - xvar;
       else xvar = xvar - yvar;
       compare_var = COMPARE(xvar, yvar);
      end
     gcd_output = xvar;
    end
   else 
    begin
     gcd_output = 8'b0;
    end
  end

endmodule
