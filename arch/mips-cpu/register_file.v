
/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

module register_file(clk, we3, a1, a2, a3, wd3, rd1, rd2);
	input clk, we3;
	input [4:0] a1, a2, a3;
	input [31:0] wd3;
	output [31:0] rd1, rd2;
	reg [31:0] REGS [31:0];

	integer i;
	initial
		begin
			for (i = 0; i < 32; i = i + 1)
				begin
					REGS[i] <= 32'd0;
				end
		end

	assign rd1 = REGS[a1];
	assign rd2 = REGS[a2];

	always @(posedge clk)
		begin
			if (we3 == 1)
				begin
					REGS[a3] = wd3;
				end
		end
endmodule
