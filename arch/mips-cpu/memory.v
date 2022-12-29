
/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

module data_memory(a, we, clk, wd, rd);
	input we, clk;
	input [31:0] a;
	input [31:0] wd;
	output [31:0] rd;

	reg [31:0] MEMORY[0:63];

	always @(posedge clk)
		begin
			if (we == 1)
				begin
					MEMORY[a >> 2] = wd;
				end
		end

	assign rd = MEMORY[a >> 2];
endmodule

module instruction_memory(a, rd);
	input [31:0] a;
	output [31:0] rd;

	reg [31:0] ram[0:63];

	initial
		begin
			$readmemb("instructions.dat", ram);
		end

	assign rd = ram[a >> 2];
endmodule
