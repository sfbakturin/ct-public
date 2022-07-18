
/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

module control_unit(opcode, funct, memtoreg, memwrite, branch, alusrc, regdst, regwrite, alucontrol);
	input [5:0] opcode, funct;
	output reg memtoreg, memwrite, branch, alusrc, regdst, regwrite;
	output reg [2:0] alucontrol;

	always @(opcode, funct)
        begin
            case(opcode)
              6'b100011:
                begin
                    regwrite <= 1;
                    regdst <= 0;
                    alusrc <= 1;
                    branch <= 0;
                    memwrite <= 0;
                    memtoreg <= 1;
                    alucontrol <= 2;
                end
              6'b101011:
                begin
                    regwrite <= 0;
                    regdst <= 1'bx;
                    alusrc <= 1;
                    branch <= 0;
                    memwrite <= 1;
                    memtoreg <= 1'bx;
                    alucontrol <= 2;
                end
              6'b000100:
                begin
                    regwrite <= 0;
                    regdst <= 1'bx;
                    alusrc <= 0;
                    branch <= 1;
                    memwrite <= 0;
                    memtoreg <= 1'bx;
                    alucontrol <= 6;
                end
              6'b001000:
                begin
                    regwrite <= 1;
                    regdst <= 0;
                    alusrc <= 1;
                    branch <= 0;
                    memwrite <= 0;
                    memtoreg <= 0;
                    alucontrol <= 2;
                end
              6'b000000:
                begin
                    regwrite <= 1;
                    regdst <= 1;
                    alusrc <= 0;
                    branch <= 0;
                    memwrite <= 0;
                    memtoreg <= 0;
                    case(funct)
                        6'b100000:
                            begin
                                alucontrol <= 3'b010;
                            end
                        6'b100010:
                            begin
                                alucontrol <= 3'b110;
                            end
                        6'b100100:
                            begin
                                alucontrol <= 3'b000;
                            end
                        6'b100101:
                            begin
                                alucontrol <= 3'b001;
                            end
                        6'b101010:
                            begin
                                alucontrol <= 3'b111;
                            end
                    endcase
                end
            endcase
        end
endmodule
