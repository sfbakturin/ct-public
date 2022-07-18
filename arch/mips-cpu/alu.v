
/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

module alu(srca, srcb, alucontrol, aluresult, zero);
    input signed [31:0] srca, srcb;
	input [2:0] alucontrol;
	output reg [31:0] aluresult;
	output reg zero;

	always @(alucontrol, srca, srcb)
		begin
			case (alucontrol)
				3'b000:
					begin
						aluresult <= srca & srcb;
					end
				3'b001:
					begin
						aluresult <= srca | srcb;
					end
				3'b010:
					begin
						aluresult <= srca + srcb;
					end
				3'b100:
					begin
						aluresult <= srca & (~srcb);
					end
				3'b101:
					begin
						aluresult <= srca | (~srcb);
					end
				3'b110:
					begin
						aluresult <= srca - srcb;
					end
				3'b111:
					begin
					    if (srca < srcb)
                            begin
                                aluresult <= 1;
                            end
                        else
                            begin
                                aluresult <= 0;
                            end
					end
			endcase
		end
    always @(aluresult)
        begin
	        if (aluresult == 0)
		        begin
			        zero <= 1;
		        end
	        else
		        begin
			        zero <= 0;
		        end
        end
endmodule
