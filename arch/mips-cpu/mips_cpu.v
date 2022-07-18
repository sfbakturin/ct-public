`include "alu.v"
`include "control_unit.v"
`include "util.v"

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

module mips_cpu(clk, instruction_memory_a, instruction_memory_rd, data_memory_a, data_memory_rd, data_memory_we, data_memory_wd,
                register_a1, register_a2, register_a3, register_we3, register_wd3, register_rd1, register_rd2);
    input clk;
    output data_memory_we;
    output [31:0] instruction_memory_a, data_memory_a, data_memory_wd;
    inout [31:0] instruction_memory_rd, data_memory_rd;
    output register_we3;
    output [4:0] register_a1, register_a2, register_a3;
    output [31:0] register_wd3;
    inout [31:0] register_rd1, register_rd2;

    wire MEMTOREG, MEMWRITE, BRANCH, ALUSRC, REGDST, REGWRITE, ZERO, ZEROBRANCH;
    wire [31:0] SRCA, SRCB, ALURESULT, SIGNEXTENDED, TEMP_PCPLUS4, TEMP_SHL2RESULT, TEMP_PCBRANCH, PC;
    wire [2:0] ALUCONTROL;

    control_unit CONTROLUNIT(instruction_memory_rd[31:26], instruction_memory_rd[5:0], MEMTOREG, data_memory_we, BRANCH, ALUSRC, REGDST, register_we3, ALUCONTROL);
    sign_extend SIGNEXTEND(instruction_memory_rd[15:0], SIGNEXTENDED);

    assign register_a1 = instruction_memory_rd[25:21];
    assign register_a2 = instruction_memory_rd[20:16];

    mux2_5 MUX_RegDst(instruction_memory_rd[20:16], instruction_memory_rd[15:11], REGDST, register_a3);

    assign SRCA = register_rd1;

    mux2_32 MUX_AluSrc(register_rd2, SIGNEXTENDED, ALUSRC, SRCB);

    alu ALU_AluControl(SRCA, SRCB, ALUCONTROL, ALURESULT, ZERO);

    assign ZEROBRANCH = ZERO & BRANCH;

    assign data_memory_wd = register_rd2;

    assign data_memory_a = ALURESULT;

    mux2_32 MUX_MemtoReg(ALURESULT, data_memory_rd, MEMTOREG, register_wd3);

    shl_2 temp0(SIGNEXTENDED, TEMP_SHL2RESULT);
    adder temp1(instruction_memory_a, 4, TEMP_PCPLUS4);
    adder temp2(TEMP_SHL2RESULT, TEMP_PCPLUS4, TEMP_PCBRANCH);

    mux2_32 MUX_PC(TEMP_PCPLUS4, TEMP_PCBRANCH, ZEROBRANCH, PC);

    d_flop CPU_FLOP(PC, clk, instruction_memory_a);
endmodule
