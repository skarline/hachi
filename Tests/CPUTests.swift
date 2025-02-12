import Testing

@testable import HachiCore

struct CPUTests {
    @Suite("Addressing Modes") struct AddressingModeTests {
        @Test("Zero Page Addressing") func zeroPageAddressing() throws {
            let cpu = CPU()
            cpu.bus.write(0x42, at: 0x34)

            cpu.execute(.lda(.zeroPage(0x34)))
            #expect(cpu.registers.a == 0x42)
        }

        @Test("Zero Page X Addressing") func zeroPageXAddressing() throws {
            let cpu = CPU()
            cpu.registers.x = 0x02
            cpu.bus.write(0x42, at: 0x36)

            cpu.execute(.lda(.zeroPageX(0x34)))
            #expect(cpu.registers.a == 0x42)
        }

        @Test("Zero Page Y Addressing") func zeroPageYAddressing() throws {
            let cpu = CPU()
            cpu.registers.y = 0x02
            cpu.bus.write(0x42, at: 0x36)

            cpu.execute(.ldx(.zeroPageY(0x34)))
            #expect(cpu.registers.x == 0x42)
        }

        @Test("Absolute Addressing") func absoluteAddressing() throws {
            let cpu = CPU()
            cpu.bus.write(0x42, at: 0x1234)

            cpu.execute(.lda(.absolute(0x1234)))
            #expect(cpu.registers.a == 0x42)
        }

        @Test("Absolute X Addressing") func absoluteXAddressing() throws {
            let cpu = CPU()
            cpu.registers.x = 0x02
            cpu.bus.write(0x42, at: 0x1236)

            cpu.execute(.lda(.absoluteX(0x1234)))
            #expect(cpu.registers.a == 0x42)
        }

        @Test("Absolute Y Addressing") func absoluteYAddressing() throws {
            let cpu = CPU()
            cpu.registers.y = 0x02
            cpu.bus.write(0x42, at: 0x1236)

            cpu.execute(.lda(.absoluteY(0x1234)))
            #expect(cpu.registers.a == 0x42)
        }

        @Test("Indirect X Addressing") func indirectXAddressing() throws {
            let cpu = CPU()
            cpu.registers.x = 0x02
            cpu.bus.writeWord(0x1234, at: 0x36)
            cpu.bus.write(0x42, at: 0x1234)

            cpu.execute(.lda(.indirectX(0x34)))
            #expect(cpu.registers.a == 0x42)
        }

        @Test("Indirect Y Addressing") func indirectYAddressing() throws {
            let cpu = CPU()
            cpu.registers.y = 0x02
            cpu.bus.writeWord(0x1234, at: 0x34)
            cpu.bus.write(0x42, at: 0x1236)

            cpu.execute(.lda(.indirectY(0x34)))
            #expect(cpu.registers.a == 0x42)
        }
    }

    @Suite("Access Instructions") struct LoadStoreTests {
        @Test("LDA Immediate") func ldaImmediate() throws {
            let cpu = CPU()
            cpu.execute(.lda(.immediate(0x42)))
            #expect(cpu.registers.a == 0x42)
            #expect(!cpu.status.contains(.zero))
            #expect(!cpu.status.contains(.negative))
        }

        @Test("LDA Zero Flag") func ldaZeroFlag() throws {
            let cpu = CPU()
            cpu.execute(.lda(.immediate(0x00)))
            #expect(cpu.registers.a == 0x00)
            #expect(cpu.status.contains(.zero))
            #expect(!cpu.status.contains(.negative))
        }

        @Test("LDA Negative Flag") func ldaNegativeFlag() throws {
            let cpu = CPU()
            cpu.execute(.lda(.immediate(0x80)))
            #expect(cpu.registers.a == 0x80)
            #expect(!cpu.status.contains(.zero))
            #expect(cpu.status.contains(.negative))
        }
    }

    @Suite("Transfer Instructions") struct TransferTests {
        @Test("TAX") func tax() throws {
            let cpu = CPU()
            cpu.registers.a = 0x42
            cpu.execute(.tax)
            #expect(cpu.registers.x == 0x42)
            #expect(!cpu.status.contains(.zero))
            #expect(!cpu.status.contains(.negative))
        }

        @Test("TAX Zero Flag") func taxZeroFlag() throws {
            let cpu = CPU()
            cpu.registers.a = 0x00
            cpu.execute(.tax)
            #expect(cpu.registers.x == 0x00)
            #expect(cpu.status.contains(.zero))
            #expect(!cpu.status.contains(.negative))
        }

        @Test("TAX Negative Flag") func taxNegativeFlag() throws {
            let cpu = CPU()
            cpu.registers.a = 0x80
            cpu.execute(.tax)
            #expect(cpu.registers.x == 0x80)
            #expect(!cpu.status.contains(.zero))
            #expect(cpu.status.contains(.negative))
        }

        @Test("TXA") func txa() throws {
            let cpu = CPU()
            cpu.registers.x = 0x42
            cpu.execute(.txa)
            #expect(cpu.registers.a == 0x42)
            #expect(!cpu.status.contains(.zero))
            #expect(!cpu.status.contains(.negative))
        }

        @Test("TAY") func tay() throws {
            let cpu = CPU()
            cpu.registers.a = 0x42
            cpu.execute(.tay)
            #expect(cpu.registers.y == 0x42)
            #expect(!cpu.status.contains(.zero))
            #expect(!cpu.status.contains(.negative))
        }

        @Test("TYA") func tya() throws {
            let cpu = CPU()
            cpu.registers.y = 0x42
            cpu.execute(.tya)
            #expect(cpu.registers.a == 0x42)
            #expect(!cpu.status.contains(.zero))
            #expect(!cpu.status.contains(.negative))
        }

        @Test("TSX") func tsx() throws {
            let cpu = CPU()
            cpu.stackPointer = 0x42
            cpu.execute(.tsx)
            #expect(cpu.registers.x == 0x42)
            #expect(!cpu.status.contains(.zero))
            #expect(!cpu.status.contains(.negative))
        }
    }

    @Suite("Arithmetic Instructions") struct ArithmeticTests {
        @Test("ADC Basic Addition") func adcBasic() throws {
            let cpu = CPU()
            cpu.registers.a = 0x01
            cpu.execute(.adc(.immediate(0x01)))
            #expect(cpu.registers.a == 0x02)
            #expect(!cpu.status.contains(.carry))
            #expect(!cpu.status.contains(.overflow))
        }

        @Test("ADC with Carry") func adcWithCarry() throws {
            let cpu = CPU()
            cpu.registers.a = 0xFF
            cpu.execute(.adc(.immediate(0x01)))
            #expect(cpu.registers.a == 0x00)
            #expect(cpu.status.contains(.carry))
            #expect(!cpu.status.contains(.overflow))
            #expect(cpu.status.contains(.zero))
        }

        @Test("ADC with Overflow") func adcWithOverflow() throws {
            let cpu = CPU()
            cpu.registers.a = 0x7F
            cpu.execute(.adc(.immediate(0x01)))
            #expect(cpu.registers.a == 0x80)
            #expect(!cpu.status.contains(.carry))
            #expect(cpu.status.contains(.overflow))
            #expect(cpu.status.contains(.negative))
        }

        @Test("SBC Basic Subtraction") func sbcBasic() throws {
            let cpu = CPU()
            cpu.registers.a = 0x42
            cpu.status.insert(.carry)
            cpu.execute(.sbc(.immediate(0x01)))
            #expect(cpu.registers.a == 0x41)
            #expect(cpu.status.contains(.carry))
            #expect(!cpu.status.contains(.overflow))
        }

        @Test("SBC with Borrow") func sbcWithBorrow() throws {
            let cpu = CPU()
            cpu.registers.a = 0x00
            cpu.status.insert(.carry)
            cpu.execute(.sbc(.immediate(0x01)))
            #expect(cpu.registers.a == 0xFF)
            #expect(!cpu.status.contains(.carry))
            #expect(!cpu.status.contains(.overflow))
            #expect(cpu.status.contains(.negative))
        }

        @Test("SBC with Overflow") func sbcWithOverflow() throws {
            let cpu = CPU()
            cpu.registers.a = 0x80
            cpu.status.insert(.carry)
            cpu.execute(.sbc(.immediate(0x01)))
            #expect(cpu.registers.a == 0x7F)
            #expect(cpu.status.contains(.carry))
            #expect(cpu.status.contains(.overflow))
            #expect(!cpu.status.contains(.negative))
        }

        @Test("INC Zero Page") func incZeroPage() throws {
            let cpu = CPU()
            cpu.bus.write(0x41, at: 0x34)
            cpu.execute(.inc(.zeroPage(0x34)))
            #expect(cpu.bus.read(at: 0x34) == 0x42)
            #expect(!cpu.status.contains(.zero))
            #expect(!cpu.status.contains(.negative))
        }

        @Test("INC Zero Flag") func incZeroFlag() throws {
            let cpu = CPU()
            cpu.bus.write(0xFF, at: 0x34)
            cpu.execute(.inc(.zeroPage(0x34)))
            #expect(cpu.bus.read(at: 0x34) == 0x00)
            #expect(cpu.status.contains(.zero))
            #expect(!cpu.status.contains(.negative))
        }

        @Test("DEC Zero Page") func decZeroPage() throws {
            let cpu = CPU()
            cpu.bus.write(0x43, at: 0x34)
            cpu.execute(.dec(.zeroPage(0x34)))
            #expect(cpu.bus.read(at: 0x34) == 0x42)
            #expect(!cpu.status.contains(.zero))
            #expect(!cpu.status.contains(.negative))
        }

        @Test("INX") func inx() throws {
            let cpu = CPU()
            cpu.registers.x = 0x41
            cpu.execute(.inx)
            #expect(cpu.registers.x == 0x42)
            #expect(!cpu.status.contains(.zero))
            #expect(!cpu.status.contains(.negative))
        }

        @Test("DEX") func dex() throws {
            let cpu = CPU()
            cpu.registers.x = 0x43
            cpu.execute(.dex)
            #expect(cpu.registers.x == 0x42)
            #expect(!cpu.status.contains(.zero))
            #expect(!cpu.status.contains(.negative))
        }

        @Test("INY") func iny() throws {
            let cpu = CPU()
            cpu.registers.y = 0x41
            cpu.execute(.iny)
            #expect(cpu.registers.y == 0x42)
            #expect(!cpu.status.contains(.zero))
            #expect(!cpu.status.contains(.negative))
        }

        @Test("DEY") func dey() throws {
            let cpu = CPU()
            cpu.registers.y = 0x43
            cpu.execute(.dey)
            #expect(cpu.registers.y == 0x42)
            #expect(!cpu.status.contains(.zero))
            #expect(!cpu.status.contains(.negative))
        }
    }

    @Suite("Shift Instructions") struct ShiftTests {
        @Test("ASL Accumulator") func aslAccumulator() throws {
            let cpu = CPU()
            cpu.registers.a = 0b01010101
            cpu.execute(.asl(.accumulator))
            #expect(cpu.registers.a == 0b10101010)
            #expect(!cpu.status.contains(.carry))
            #expect(cpu.status.contains(.negative))
        }

        @Test("ASL Memory") func aslMemory() throws {
            let cpu = CPU()
            cpu.bus.write(0b01010101, at: 0x34)
            cpu.execute(.asl(.zeroPage(0x34)))
            #expect(cpu.bus.read(at: 0x34) == 0b10101010)
            #expect(!cpu.status.contains(.carry))
            #expect(cpu.status.contains(.negative))
        }

        @Test("LSR Accumulator") func lsrAccumulator() throws {
            let cpu = CPU()
            cpu.registers.a = 0b01010101
            cpu.execute(.lsr(.accumulator))
            #expect(cpu.registers.a == 0b00101010)
            #expect(cpu.status.contains(.carry))
            #expect(!cpu.status.contains(.negative))
        }

        @Test("LSR Memory") func lsrMemory() throws {
            let cpu = CPU()
            cpu.bus.write(0b01010101, at: 0x34)
            cpu.execute(.lsr(.zeroPage(0x34)))
            #expect(cpu.bus.read(at: 0x34) == 0b00101010)
            #expect(cpu.status.contains(.carry))
            #expect(!cpu.status.contains(.negative))
        }

        @Test("ROL Accumulator") func rolAccumulator() throws {
            let cpu = CPU()
            cpu.registers.a = 0b01010101
            cpu.status.insert(.carry)
            cpu.execute(.rol(.accumulator))
            #expect(cpu.registers.a == 0b10101011)
            #expect(!cpu.status.contains(.carry))
            #expect(cpu.status.contains(.negative))
        }

        @Test("ROR Accumulator") func rorAccumulator() throws {
            let cpu = CPU()
            cpu.registers.a = 0b01010101
            cpu.status.insert(.carry)
            cpu.execute(.ror(.accumulator))
            #expect(cpu.registers.a == 0b10101010)
            #expect(cpu.status.contains(.carry))
            #expect(cpu.status.contains(.negative))
        }
    }

    @Suite("Bitwise Instructions") struct BitwiseTests {
        @Test("AND Basic") func andBasic() throws {
            let cpu = CPU()
            cpu.registers.a = 0b11110000
            cpu.execute(.and(.immediate(0b00111100)))
            #expect(cpu.registers.a == 0b00110000)
        }

        @Test("ORA Basic") func oraBasic() throws {
            let cpu = CPU()
            cpu.registers.a = 0b11110000
            cpu.execute(.ora(.immediate(0b00111100)))
            #expect(cpu.registers.a == 0b11111100)
        }

        @Test("EOR Basic") func eorBasic() throws {
            let cpu = CPU()
            cpu.registers.a = 0b11110000
            cpu.execute(.eor(.immediate(0b00111100)))
            #expect(cpu.registers.a == 0b11001100)
            #expect(!cpu.status.contains(.zero))
            #expect(cpu.status.contains(.negative))
        }

        @Test("BIT Zero Page") func bitZeroPage() throws {
            let cpu = CPU()
            cpu.registers.a = 0b11110000
            cpu.bus.write(0b11110000, at: 0x34)
            cpu.execute(.bit(.zeroPage(0x34)))
            #expect(!cpu.status.contains(.zero))
            #expect(cpu.status.contains(.negative))
            #expect(cpu.status.contains(.overflow))
        }
    }

    @Suite("Compare Instructions") struct CompareTests {
        @Test("CMP Equal") func cmpEqual() throws {
            let cpu = CPU()
            cpu.registers.a = 0x42
            cpu.execute(.cmp(.immediate(0x42)))
            #expect(cpu.status.contains(.carry))
            #expect(cpu.status.contains(.zero))
            #expect(!cpu.status.contains(.negative))
        }

        @Test("CPX Greater") func cpxGreater() throws {
            let cpu = CPU()
            cpu.registers.x = 0x42
            cpu.execute(.cpx(.immediate(0x40)))
            #expect(cpu.status.contains(.carry))
            #expect(!cpu.status.contains(.zero))
            #expect(!cpu.status.contains(.negative))
        }

        @Test("CPY Less") func cpyLess() throws {
            let cpu = CPU()
            cpu.registers.y = 0x40
            cpu.execute(.cpy(.immediate(0x42)))
            #expect(!cpu.status.contains(.carry))
            #expect(!cpu.status.contains(.zero))
            #expect(cpu.status.contains(.negative))
        }
    }

    @Suite("Branch Instructions") struct BranchTests {
        @Test("BCC Takes Branch") func bccTakesBranch() throws {
            let cpu = CPU()
            cpu.programCounter = 0x1000
            cpu.execute(.bcc(.relative(0x10)))
            #expect(cpu.programCounter == 0x1010)
        }

        @Test("BCS Takes Branch") func bcsTakesBranch() throws {
            let cpu = CPU()
            cpu.status.insert(.carry)
            cpu.programCounter = 0x1000
            cpu.execute(.bcs(.relative(0x10)))
            #expect(cpu.programCounter == 0x1010)
        }

        @Test("BEQ Takes Branch") func beqTakesBranch() throws {
            let cpu = CPU()
            cpu.status.insert(.zero)
            cpu.programCounter = 0x1000
            cpu.execute(.beq(.relative(0x10)))
            #expect(cpu.programCounter == 0x1010)
        }

        @Test("BMI Takes Branch") func bmiTakesBranch() throws {
            let cpu = CPU()
            cpu.status.insert(.negative)
            cpu.programCounter = 0x1000
            cpu.execute(.bmi(.relative(0x10)))
            #expect(cpu.programCounter == 0x1010)
        }

        @Test("BNE Takes Branch") func bneTakesBranch() throws {
            let cpu = CPU()
            cpu.programCounter = 0x1000
            cpu.execute(.bne(.relative(0x10)))
            #expect(cpu.programCounter == 0x1010)
        }

        @Test("BPL Takes Branch") func bplTakesBranch() throws {
            let cpu = CPU()
            cpu.programCounter = 0x1000
            cpu.execute(.bpl(.relative(0x10)))
            #expect(cpu.programCounter == 0x1010)
        }

        @Test("BVC Takes Branch") func bvcTakesBranch() throws {
            let cpu = CPU()
            cpu.programCounter = 0x1000
            cpu.execute(.bvc(.relative(0x10)))
            #expect(cpu.programCounter == 0x1010)
        }

        @Test("BVS Takes Branch") func bvsTakesBranch() throws {
            let cpu = CPU()
            cpu.status.insert(.overflow)
            cpu.programCounter = 0x1000
            cpu.execute(.bvs(.relative(0x10)))
            #expect(cpu.programCounter == 0x1010)
        }

        @Test("BCC Does Not Take Branch") func bccDoesNotTakeBranch() throws {
            let cpu = CPU()
            cpu.status.insert(.carry)
            cpu.programCounter = 0x1000
            cpu.execute(.bcc(.relative(0x10)))
            #expect(cpu.programCounter == 0x1000)
        }

        @Test("BCS Does Not Take Branch") func bcsDoesNotTakeBranch() throws {
            let cpu = CPU()
            cpu.programCounter = 0x1000
            cpu.execute(.bcs(.relative(0x10)))
            #expect(cpu.programCounter == 0x1000)
        }

        @Test("BEQ Does Not Take Branch") func beqDoesNotTakeBranch() throws {
            let cpu = CPU()
            cpu.programCounter = 0x1000
            cpu.execute(.beq(.relative(0x10)))
            #expect(cpu.programCounter == 0x1000)
        }

        @Test("BMI Does Not Take Branch") func bmiDoesNotTakeBranch() throws {
            let cpu = CPU()
            cpu.programCounter = 0x1000
            cpu.execute(.bmi(.relative(0x10)))
            #expect(cpu.programCounter == 0x1000)
        }

        @Test("BNE Does Not Take Branch") func bneDoesNotTakeBranch() throws {
            let cpu = CPU()
            cpu.status.insert(.zero)
            cpu.programCounter = 0x1000
            cpu.execute(.bne(.relative(0x10)))
            #expect(cpu.programCounter == 0x1000)
        }

        @Test("BPL Does Not Take Branch") func bplDoesNotTakeBranch() throws {
            let cpu = CPU()
            cpu.status.insert(.negative)
            cpu.programCounter = 0x1000
            cpu.execute(.bpl(.relative(0x10)))
            #expect(cpu.programCounter == 0x1000)
        }

        @Test("Branch with Negative Offset") func branchWithNegativeOffset() throws {
            let cpu = CPU()
            cpu.programCounter = 0x1000
            cpu.execute(.bcc(.relative(0xF0)))
            #expect(cpu.programCounter == 0x0FF0)
        }

        @Test("Branch Crosses Page Boundary") func branchCrossesPageBoundary() throws {
            let cpu = CPU()
            cpu.programCounter = 0x10F0
            cpu.execute(.bcc(.relative(0x20)))
            #expect(cpu.programCounter == 0x1110)
        }
    }

    @Suite("Jump Instructions") struct JumpTests {
        @Test("JMP Absolute") func jmpAbsolute() throws {
            let cpu = CPU()
            cpu.execute(.jmp(.absolute(0x1234)))
            #expect(cpu.programCounter == 0x1234)
        }

        @Test("JMP Indirect") func jmpIndirect() throws {
            let cpu = CPU()

            cpu.bus.writeWord(0x4321, at: 0x1000)

            cpu.execute(.jmp(.indirect(0x1000)))
            #expect(cpu.programCounter == 0x4321)
        }

        @Test("JMP Indirect Page Boundary Bug") func jmpIndirectPageBoundaryBug() throws {
            let cpu = CPU()

            cpu.bus.write(0x21, at: UInt16(0x02FF))
            cpu.bus.write(0x43, at: UInt16(0x0200))

            cpu.execute(.jmp(.indirect(0x02FF)))
            #expect(cpu.programCounter == 0x4321)
        }

        @Test("JSR") func jsr() throws {
            let cpu = CPU()
            cpu.programCounter = 0x1000
            cpu.stackPointer = 0xFF
            cpu.execute(.jsr(.absolute(0x2000)))
            #expect(cpu.programCounter == 0x2000)
            #expect(cpu.stackPointer == 0xFD)
            #expect(cpu.bus.readWord(at: 0x01FE) == 0x1000)
        }

        @Test("RTS") func rts() throws {
            let cpu = CPU()
            cpu.stackPointer = 0xFD
            cpu.bus.writeWord(0x1000, at: 0x01FE)
            cpu.execute(.rts)
            #expect(cpu.programCounter == 0x1000)
            #expect(cpu.stackPointer == 0xFF)
        }

        @Test("BRK") func brk() throws {
            let cpu = CPU()
            cpu.programCounter = 0x1000
            cpu.stackPointer = 0xFF
            cpu.status = [.carry, .zero]
            cpu.bus.writeWord(0x1000, at: 0xFFFE)
            cpu.execute(.brk)
            #expect(cpu.programCounter == 0x1000)
            #expect(cpu.stackPointer == 0xFC)
            #expect(cpu.status.contains(.interruptDisable))
        }

        @Test("RTI") func rti() throws {
            let cpu = CPU()
            cpu.stackPointer = 0xFC
            cpu.bus.write(0x03, at: 0x01FD)
            cpu.bus.writeWord(0x1000, at: 0x01FE)
            cpu.execute(.rti)
            #expect(cpu.programCounter == 0x1000)
            #expect(cpu.stackPointer == 0xFF)
            #expect(cpu.status.contains(.carry))
            #expect(cpu.status.contains(.zero))
        }
    }

    @Suite("Stack Instructions") struct StackTests {
        @Test("PHA") func pha() throws {
            let cpu = CPU()
            cpu.registers.a = 0x42
            cpu.stackPointer = 0xFF
            cpu.execute(.pha)
            #expect(cpu.stackPointer == 0xFE)
            #expect(cpu.bus.read(at: 0x01FF) == 0x42)
        }

        @Test("PHP") func php() throws {
            let cpu = CPU()
            cpu.status = [.carry, .zero]
            cpu.stackPointer = 0xFF
            cpu.execute(.php)
            #expect(cpu.stackPointer == 0xFE)
            #expect(cpu.bus.read(at: 0x01FF) == 0x03)
        }

        @Test("PLA") func pla() throws {
            let cpu = CPU()
            cpu.stackPointer = 0xFE
            cpu.bus.write(0x42, at: 0x01FF)
            cpu.execute(.pla)
            #expect(cpu.stackPointer == 0xFF)
            #expect(cpu.registers.a == 0x42)
            #expect(!cpu.status.contains(.zero))
            #expect(!cpu.status.contains(.negative))
        }

        @Test("PLP") func plp() throws {
            let cpu = CPU()
            cpu.stackPointer = 0xFE
            cpu.bus.write(0x03, at: 0x01FF)
            cpu.execute(.plp)
            #expect(cpu.stackPointer == 0xFF)
            #expect(cpu.status.contains(.carry))
            #expect(cpu.status.contains(.zero))
        }
    }

    @Suite("Flag Instructions") struct FlagTests {
        @Test("CLC") func clc() throws {
            let cpu = CPU()
            cpu.status.insert(.carry)
            cpu.execute(.clc)
            #expect(!cpu.status.contains(.carry))
        }

        @Test("CLD") func cld() throws {
            let cpu = CPU()
            cpu.status.insert(.decimal)
            cpu.execute(.cld)
            #expect(!cpu.status.contains(.decimal))
        }

        @Test("CLI") func cli() throws {
            let cpu = CPU()
            cpu.status.insert(.interruptDisable)
            cpu.execute(.cli)
            #expect(!cpu.status.contains(.interruptDisable))
        }

        @Test("CLV") func clv() throws {
            let cpu = CPU()
            cpu.status.insert(.overflow)
            cpu.execute(.clv)
            #expect(!cpu.status.contains(.overflow))
        }

        @Test("SEC") func sec() throws {
            let cpu = CPU()
            cpu.execute(.sec)
            #expect(cpu.status.contains(.carry))
        }

        @Test("SED") func sed() throws {
            let cpu = CPU()
            cpu.execute(.sed)
            #expect(cpu.status.contains(.decimal))
        }

        @Test("SEI") func sei() throws {
            let cpu = CPU()
            cpu.execute(.sei)
            #expect(cpu.status.contains(.interruptDisable))
        }
    }

    @Suite("Store Instructions") struct StoreTests {
        @Test("STA Zero Page") func staZeroPage() throws {
            let cpu = CPU()
            cpu.registers.a = 0x42
            cpu.execute(.sta(.zeroPage(0x34)))
            #expect(cpu.bus.read(at: 0x34) == 0x42)
        }

        @Test("STA Absolute") func staAbsolute() throws {
            let cpu = CPU()
            cpu.registers.a = 0x42
            cpu.execute(.sta(.absolute(0x1234)))
            #expect(cpu.bus.read(at: 0x1234) == 0x42)
        }

        @Test("STX Zero Page") func stxZeroPage() throws {
            let cpu = CPU()
            cpu.registers.x = 0x42
            cpu.execute(.stx(.zeroPage(0x34)))
            #expect(cpu.bus.read(at: 0x34) == 0x42)
        }

        @Test("STX Zero Page Y") func stxZeroPageY() throws {
            let cpu = CPU()
            cpu.registers.x = 0x42
            cpu.registers.y = 0x02
            cpu.execute(.stx(.zeroPageY(0x34)))
            #expect(cpu.bus.read(at: 0x36) == 0x42)
        }

        @Test("STY Zero Page") func styZeroPage() throws {
            let cpu = CPU()
            cpu.registers.y = 0x42
            cpu.execute(.sty(.zeroPage(0x34)))
            #expect(cpu.bus.read(at: 0x34) == 0x42)
        }

        @Test("STY Zero Page X") func styZeroPageX() throws {
            let cpu = CPU()
            cpu.registers.y = 0x42
            cpu.registers.x = 0x02
            cpu.execute(.sty(.zeroPageX(0x34)))
            #expect(cpu.bus.read(at: 0x36) == 0x42)
        }

        @Test("STA Absolute X") func staAbsoluteX() throws {
            let cpu = CPU()
            cpu.registers.a = 0x42
            cpu.registers.x = 0x02
            cpu.execute(.sta(.absoluteX(0x1234)))
            #expect(cpu.bus.read(at: 0x1236) == 0x42)
        }

        @Test("STA Absolute Y") func staAbsoluteY() throws {
            let cpu = CPU()
            cpu.registers.a = 0x42
            cpu.registers.y = 0x02
            cpu.execute(.sta(.absoluteY(0x1234)))
            #expect(cpu.bus.read(at: 0x1236) == 0x42)
        }

        @Test("STA Indirect X") func staIndirectX() throws {
            let cpu = CPU()
            cpu.registers.a = 0x42
            cpu.registers.x = 0x02
            cpu.bus.writeWord(0x1234, at: 0x36)
            cpu.execute(.sta(.indirectX(0x34)))
            #expect(cpu.bus.read(at: 0x1234) == 0x42)
        }

        @Test("STA Indirect Y") func staIndirectY() throws {
            let cpu = CPU()
            cpu.registers.a = 0x42
            cpu.registers.y = 0x02
            cpu.bus.writeWord(0x1234, at: 0x34)
            cpu.execute(.sta(.indirectY(0x34)))
            #expect(cpu.bus.read(at: 0x1236) == 0x42)
        }

        @Test("STX Absolute") func stxAbsolute() throws {
            let cpu = CPU()
            cpu.registers.x = 0x42
            cpu.execute(.stx(.absolute(0x1234)))
            #expect(cpu.bus.read(at: 0x1234) == 0x42)
        }

        @Test("STY Absolute") func styAbsolute() throws {
            let cpu = CPU()
            cpu.registers.y = 0x42
            cpu.execute(.sty(.absolute(0x1234)))
            #expect(cpu.bus.read(at: 0x1234) == 0x42)
        }
    }

    @Suite("Memory Operations") struct MemoryTests {
        @Test("Zero Page Wrap Around") func zeroPageWrapAround() throws {
            let cpu = CPU()
            cpu.registers.x = 0xFF
            cpu.bus.write(0x42, at: 0x33)
            cpu.execute(.lda(.zeroPageX(0x34)))
            #expect(cpu.registers.a == 0x42)
        }

        @Test("Absolute X Page Cross") func absoluteXPageCross() throws {
            let cpu = CPU()
            cpu.registers.x = 0xFF
            cpu.bus.write(0x42, at: 0x1234)
            cpu.execute(.lda(.absoluteX(0x1135)))
            #expect(cpu.registers.a == 0x42)
        }

        @Test("Absolute Y Page Cross") func absoluteYPageCross() throws {
            let cpu = CPU()
            cpu.registers.y = 0xFF
            cpu.bus.write(0x42, at: 0x1234)
            cpu.execute(.lda(.absoluteY(0x1135)))
            #expect(cpu.registers.a == 0x42)
        }

        @Test("Indirect Y Page Cross") func indirectYPageCross() throws {
            let cpu = CPU()
            cpu.registers.y = 0xFF
            cpu.bus.writeWord(0x1135, at: 0x34)
            cpu.bus.write(0x42, at: 0x1234)
            cpu.execute(.lda(.indirectY(0x34)))
            #expect(cpu.registers.a == 0x42)
        }
    }
}
