public class CPU {
    struct Registers {
        var a: UInt8 = 0
        var x: UInt8 = 0
        var y: UInt8 = 0
    }

    struct StatusFlags: OptionSet {
        let rawValue: UInt8

        static let carry =
            StatusFlags(rawValue: 1 << 0)
        static let zero =
            StatusFlags(rawValue: 1 << 1)
        static let interruptDisable =
            StatusFlags(rawValue: 1 << 2)
        static let decimal =
            StatusFlags(rawValue: 1 << 3)
        static let breakCommand =
            StatusFlags(rawValue: 1 << 4)
        static let unused =
            StatusFlags(rawValue: 1 << 5)
        static let overflow =
            StatusFlags(rawValue: 1 << 6)
        static let negative =
            StatusFlags(rawValue: 1 << 7)

        mutating func set(_ flag: Self, to value: Bool) {
            if value { insert(flag) } else { remove(flag) }
        }

        mutating func set<T: BinaryInteger>(_ flag: Self, to value: T) {
            set(flag, to: value != 0)
        }
    }

    enum Instruction {
        // Access
        case lda(AddressingMode)
        case ldx(AddressingMode)
        case ldy(AddressingMode)
        case sta(AddressingMode)
        case stx(AddressingMode)
        case sty(AddressingMode)

        // Transfer
        case tax
        case txa
        case tay
        case tya
        case tsx
        case txs

        // Arithmetic
        case adc(AddressingMode)
        case sbc(AddressingMode)
        case inc(AddressingMode)
        case dec(AddressingMode)
        case inx
        case iny
        case dex
        case dey

        // Shift
        case asl(AddressingMode)
        case lsr(AddressingMode)
        case rol(AddressingMode)
        case ror(AddressingMode)

        // Bitwise
        case and(AddressingMode)
        case ora(AddressingMode)
        case eor(AddressingMode)
        case bit(AddressingMode)

        // Compare
        case cmp(AddressingMode)
        case cpx(AddressingMode)
        case cpy(AddressingMode)

        // Branch
        case bcc(AddressingMode)
        case bcs(AddressingMode)
        case beq(AddressingMode)
        case bmi(AddressingMode)
        case bne(AddressingMode)
        case bpl(AddressingMode)
        case bvc(AddressingMode)
        case bvs(AddressingMode)

        // Jump
        case jmp(AddressingMode)
        case jsr(AddressingMode)
        case rts
        case brk
        case rti

        // Stack
        case pha
        case php
        case pla
        case plp

        // Flags
        case clc
        case cld
        case cli
        case clv
        case sec
        case sed
        case sei

        // Other
        case nop
    }

    enum AddressingMode {
        case implied
        case accumulator
        case immediate(UInt8)
        case zeroPage(UInt8)
        case zeroPageX(UInt8)
        case zeroPageY(UInt8)
        case absolute(UInt16)
        case absoluteX(UInt16)
        case absoluteY(UInt16)
        case indirect(UInt16)
        case indirectX(UInt8)
        case indirectY(UInt8)
        case relative(UInt8)
    }

    var bus: Bus

    var registers = Registers()
    var status: StatusFlags = [.unused]
    var stackPointer: UInt8 = 0
    var programCounter: UInt16 = 0x8000

    var remainingCycles: Int = 0

    #if DEBUG
        init() {
            self.bus = Bus()
            self.bus.connect(cpu: self)
        }
    #endif

    init(bus: Bus) {
        self.bus = bus
        self.bus.connect(cpu: self)
    }

    // MARK: - Fetch

    private func fetchByte() -> UInt8 {
        defer { programCounter &+= 1 }
        return bus.read(at: programCounter)
    }

    private func fetchWord() -> UInt16 {
        defer { programCounter &+= 2 }
        return bus.readWord(at: programCounter)
    }

    // MARK: - Decode

    func decode(_ opcode: UInt8) -> (instruction: Instruction, cycles: Int) {
        switch opcode {
        case 0xA9: (.lda(.immediate(fetchByte())), 2)
        case 0xA5: (.lda(.zeroPage(fetchByte())), 3)
        case 0xB5: (.lda(.zeroPageX(fetchByte())), 4)
        case 0xAD: (.lda(.absolute(fetchWord())), 4)
        case 0xBD: (.lda(.absoluteX(fetchWord())), 4)
        case 0xB9: (.lda(.absoluteY(fetchWord())), 4)
        case 0xA1: (.lda(.indirectX(fetchByte())), 6)
        case 0xB1: (.lda(.indirectY(fetchByte())), 5)

        case 0xA2: (.ldx(.immediate(fetchByte())), 2)
        case 0xA6: (.ldx(.zeroPage(fetchByte())), 3)
        case 0xB6: (.ldx(.zeroPageY(fetchByte())), 4)
        case 0xAE: (.ldx(.absolute(fetchWord())), 4)
        case 0xBE: (.ldx(.absoluteY(fetchWord())), 4)

        case 0xA0: (.ldy(.immediate(fetchByte())), 2)
        case 0xA4: (.ldy(.zeroPage(fetchByte())), 3)
        case 0xB4: (.ldy(.zeroPageX(fetchByte())), 4)
        case 0xAC: (.ldy(.absolute(fetchWord())), 4)
        case 0xBC: (.ldy(.absoluteX(fetchWord())), 4)

        case 0x85: (.sta(.zeroPage(fetchByte())), 3)
        case 0x95: (.sta(.zeroPageX(fetchByte())), 4)
        case 0x8D: (.sta(.absolute(fetchWord())), 4)
        case 0x9D: (.sta(.absoluteX(fetchWord())), 5)
        case 0x99: (.sta(.absoluteY(fetchWord())), 5)
        case 0x81: (.sta(.indirectX(fetchByte())), 6)
        case 0x91: (.sta(.indirectY(fetchByte())), 6)

        case 0x86: (.stx(.zeroPage(fetchByte())), 3)
        case 0x96: (.stx(.zeroPageY(fetchByte())), 4)
        case 0x8E: (.stx(.absolute(fetchWord())), 4)

        case 0x84: (.sty(.zeroPage(fetchByte())), 3)
        case 0x94: (.sty(.zeroPageX(fetchByte())), 4)
        case 0x8C: (.sty(.absolute(fetchWord())), 4)

        case 0xAA: (.tax, 2)
        case 0x8A: (.txa, 2)
        case 0xA8: (.tay, 2)
        case 0x98: (.tya, 2)
        case 0xBA: (.tsx, 2)
        case 0x9A: (.txs, 2)

        case 0x69: (.adc(.immediate(fetchByte())), 2)
        case 0x65: (.adc(.zeroPage(fetchByte())), 3)
        case 0x75: (.adc(.zeroPageX(fetchByte())), 4)
        case 0x6D: (.adc(.absolute(fetchWord())), 4)
        case 0x7D: (.adc(.absoluteX(fetchWord())), 4)
        case 0x79: (.adc(.absoluteY(fetchWord())), 4)
        case 0x61: (.adc(.indirectX(fetchByte())), 6)
        case 0x71: (.adc(.indirectY(fetchByte())), 5)

        case 0xE9: (.sbc(.immediate(fetchByte())), 2)
        case 0xE5: (.sbc(.zeroPage(fetchByte())), 3)
        case 0xF5: (.sbc(.zeroPageX(fetchByte())), 4)
        case 0xED: (.sbc(.absolute(fetchWord())), 4)
        case 0xFD: (.sbc(.absoluteX(fetchWord())), 4)
        case 0xF9: (.sbc(.absoluteY(fetchWord())), 4)
        case 0xE1: (.sbc(.indirectX(fetchByte())), 6)
        case 0xF1: (.sbc(.indirectY(fetchByte())), 5)

        case 0xE6: (.inc(.zeroPage(fetchByte())), 5)
        case 0xF6: (.inc(.zeroPageX(fetchByte())), 6)
        case 0xEE: (.inc(.absolute(fetchWord())), 6)
        case 0xFE: (.inc(.absoluteX(fetchWord())), 7)

        case 0xC6: (.dec(.zeroPage(fetchByte())), 5)
        case 0xD6: (.dec(.zeroPageX(fetchByte())), 6)
        case 0xCE: (.dec(.absolute(fetchWord())), 6)
        case 0xDE: (.dec(.absoluteX(fetchWord())), 7)

        case 0xE8: (.inx, 2)
        case 0xC8: (.iny, 2)
        case 0xCA: (.dex, 2)
        case 0x88: (.dey, 2)

        case 0x0A: (.asl(.accumulator), 2)
        case 0x06: (.asl(.zeroPage(fetchByte())), 5)
        case 0x16: (.asl(.zeroPageX(fetchByte())), 6)
        case 0x0E: (.asl(.absolute(fetchWord())), 6)
        case 0x1E: (.asl(.absoluteX(fetchWord())), 7)

        case 0x4A: (.lsr(.accumulator), 2)
        case 0x46: (.lsr(.zeroPage(fetchByte())), 5)
        case 0x56: (.lsr(.zeroPageX(fetchByte())), 6)
        case 0x4E: (.lsr(.absolute(fetchWord())), 6)
        case 0x5E: (.lsr(.absoluteX(fetchWord())), 7)

        case 0x2A: (.rol(.accumulator), 2)
        case 0x26: (.rol(.zeroPage(fetchByte())), 5)
        case 0x36: (.rol(.zeroPageX(fetchByte())), 6)
        case 0x2E: (.rol(.absolute(fetchWord())), 6)
        case 0x3E: (.rol(.absoluteX(fetchWord())), 7)

        case 0x6A: (.ror(.accumulator), 2)
        case 0x66: (.ror(.zeroPage(fetchByte())), 5)
        case 0x76: (.ror(.zeroPageX(fetchByte())), 6)
        case 0x6E: (.ror(.absolute(fetchWord())), 6)
        case 0x7E: (.ror(.absoluteX(fetchWord())), 7)

        case 0x29: (.and(.immediate(fetchByte())), 2)
        case 0x25: (.and(.zeroPage(fetchByte())), 3)
        case 0x35: (.and(.zeroPageX(fetchByte())), 4)
        case 0x2D: (.and(.absolute(fetchWord())), 4)
        case 0x3D: (.and(.absoluteX(fetchWord())), 4)
        case 0x39: (.and(.absoluteY(fetchWord())), 4)
        case 0x21: (.and(.indirectX(fetchByte())), 6)
        case 0x31: (.and(.indirectY(fetchByte())), 5)

        case 0x09: (.ora(.immediate(fetchByte())), 2)
        case 0x05: (.ora(.zeroPage(fetchByte())), 3)
        case 0x15: (.ora(.zeroPageX(fetchByte())), 4)
        case 0x0D: (.ora(.absolute(fetchWord())), 4)
        case 0x1D: (.ora(.absoluteX(fetchWord())), 4)
        case 0x19: (.ora(.absoluteY(fetchWord())), 4)
        case 0x01: (.ora(.indirectX(fetchByte())), 6)
        case 0x11: (.ora(.indirectY(fetchByte())), 5)

        case 0x49: (.eor(.immediate(fetchByte())), 2)
        case 0x45: (.eor(.zeroPage(fetchByte())), 3)
        case 0x55: (.eor(.zeroPageX(fetchByte())), 4)
        case 0x4D: (.eor(.absolute(fetchWord())), 4)
        case 0x5D: (.eor(.absoluteX(fetchWord())), 4)
        case 0x59: (.eor(.absoluteY(fetchWord())), 4)
        case 0x41: (.eor(.indirectX(fetchByte())), 6)
        case 0x51: (.eor(.indirectY(fetchByte())), 5)

        case 0x24: (.bit(.zeroPage(fetchByte())), 3)
        case 0x2C: (.bit(.absolute(fetchWord())), 4)

        case 0xC9: (.cmp(.immediate(fetchByte())), 2)
        case 0xC5: (.cmp(.zeroPage(fetchByte())), 3)
        case 0xD5: (.cmp(.zeroPageX(fetchByte())), 4)
        case 0xCD: (.cmp(.absolute(fetchWord())), 4)
        case 0xDD: (.cmp(.absoluteX(fetchWord())), 4)
        case 0xD9: (.cmp(.absoluteY(fetchWord())), 4)
        case 0xC1: (.cmp(.indirectX(fetchByte())), 6)
        case 0xD1: (.cmp(.indirectY(fetchByte())), 5)

        case 0xE0: (.cpx(.immediate(fetchByte())), 2)
        case 0xE4: (.cpx(.zeroPage(fetchByte())), 3)
        case 0xEC: (.cpx(.absolute(fetchWord())), 4)

        case 0xC0: (.cpy(.immediate(fetchByte())), 2)
        case 0xC4: (.cpy(.zeroPage(fetchByte())), 3)
        case 0xCC: (.cpy(.absolute(fetchWord())), 4)

        case 0x90: (.bcc(.relative(fetchByte())), 2)
        case 0xB0: (.bcs(.relative(fetchByte())), 2)
        case 0xF0: (.beq(.relative(fetchByte())), 2)
        case 0x30: (.bmi(.relative(fetchByte())), 2)
        case 0xD0: (.bne(.relative(fetchByte())), 2)
        case 0x10: (.bpl(.relative(fetchByte())), 2)
        case 0x50: (.bvc(.relative(fetchByte())), 2)
        case 0x70: (.bvs(.relative(fetchByte())), 2)

        case 0x4C: (.jmp(.absolute(fetchWord())), 3)
        case 0x6C: (.jmp(.indirect(fetchWord())), 5)
        case 0x20: (.jsr(.absolute(fetchWord())), 6)
        case 0x60: (.rts, 6)
        case 0x00: (.brk, 7)
        case 0x40: (.rti, 6)

        case 0x48: (.pha, 3)
        case 0x08: (.php, 3)
        case 0x68: (.pla, 4)
        case 0x28: (.plp, 4)

        case 0x18: (.clc, 2)
        case 0xD8: (.cld, 2)
        case 0x58: (.cli, 2)
        case 0xB8: (.clv, 2)
        case 0x38: (.sec, 2)
        case 0xF8: (.sed, 2)
        case 0x78: (.sei, 2)

        case 0xEA: (.nop, 2)

        default: fatalError("Illegal opcode: \(String(opcode, radix: 16, uppercase: true))")
        }
    }

    // MARK: - Execute

    private func address(for mode: AddressingMode) -> UInt16 {
        switch mode {
        case .zeroPage(let address):
            return UInt16(address)

        case .zeroPageX(let address):
            return UInt16(UInt8(address &+ registers.x))

        case .zeroPageY(let address):
            return UInt16(UInt8(address &+ registers.y))

        case .absolute(let address):
            return address

        case .absoluteX(let base):
            let address = base &+ UInt16(registers.x)
            if (base & 0xFF00) != (address & 0xFF00) {
                remainingCycles += 1
            }
            return address

        case .absoluteY(let base):
            let address = base &+ UInt16(registers.y)
            if (base & 0xFF00) != (address & 0xFF00) {
                remainingCycles += 1
            }
            return address

        case .indirect(let address):
            // Handle the page boundary bug:
            // If pointer = $xxFF, fetch second byte from $xx00 instead of $xx+1:FF
            let lo = bus.read(at: address)
            let hi = bus.read(at: (address & 0xFF00) | UInt16((address + 1) & 0xFF))
            return UInt16(hi) << 8 | UInt16(lo)

        case .indirectX(let address):
            return bus.readWord(at: address &+ registers.x)

        case .indirectY(let address):
            let base = bus.readWord(at: address)
            let address = base &+ UInt16(registers.y)
            if (base & 0xFF00) != (address & 0xFF00) {
                remainingCycles += 1
            }
            return address

        case .relative(let offset):
            let base = programCounter
            let address = base &+ UInt16(bitPattern: Int16(Int8(bitPattern: offset)))
            if (base & 0xFF00) != (address & 0xFF00) {
                remainingCycles += 1
            }
            return address

        default:
            fatalError("Illegal addressing for \(String(describing: mode))")
        }
    }

    private func operand(for mode: AddressingMode) -> UInt8 {
        switch mode {
        case .immediate(let value):
            return value

        case .accumulator:
            return registers.a

        default:
            let address = address(for: mode)
            return bus.read(at: address)
        }
    }

    private func updateOperand(for mode: AddressingMode, with value: UInt8) {
        if case .accumulator = mode {
            registers.a = value
            return
        }

        let address = address(for: mode)
        bus.write(value, at: address)
    }

    private func pushStackByte(_ value: UInt8) {
        bus.write(value, at: 0x0100 + UInt16(stackPointer))
        stackPointer &-= 1
    }

    private func pullStackByte() -> UInt8 {
        stackPointer &+= 1
        return bus.read(at: 0x0100 + UInt16(stackPointer))
    }

    private func pushStackWord(value: UInt16) {
        pushStackByte(UInt8(value >> 8))
        pushStackByte(UInt8(value & 0xFF))
    }

    private func pullStackWord() -> UInt16 {
        let lo = pullStackByte()
        let hi = pullStackByte()
        return UInt16(hi) << 8 | UInt16(lo)
    }

    private func branch(to address: UInt16) {
        remainingCycles += 1
        programCounter = address
    }

    func execute(_ instruction: Instruction) {
        switch instruction {
        case .lda(let mode):
            registers.a = operand(for: mode)
            status.set(.zero, to: registers.a == 0)
            status.set(.negative, to: registers.a & 0x80)

        case .ldx(let mode):
            registers.x = operand(for: mode)
            status.set(.zero, to: registers.x == 0)
            status.set(.negative, to: registers.x & 0x80)

        case .ldy(let mode):
            registers.y = operand(for: mode)
            status.set(.zero, to: registers.y == 0)
            status.set(.negative, to: registers.y & 0x80)

        case .sta(let mode):
            bus.write(registers.a, at: address(for: mode))

        case .stx(let mode):
            bus.write(registers.x, at: address(for: mode))

        case .sty(let mode):
            bus.write(registers.y, at: address(for: mode))

        case .tax:
            registers.x = registers.a
            status.set(.zero, to: registers.x == 0)
            status.set(.negative, to: registers.x & 0x80)

        case .txa:
            registers.a = registers.x
            status.set(.zero, to: registers.a == 0)
            status.set(.negative, to: registers.a & 0x80)

        case .tay:
            registers.y = registers.a
            status.set(.zero, to: registers.y == 0)
            status.set(.negative, to: registers.y & 0x80)

        case .tya:
            registers.a = registers.y
            status.set(.zero, to: registers.a == 0)
            status.set(.negative, to: registers.a & 0x80)

        case .tsx:
            registers.x = stackPointer
            status.set(.zero, to: registers.x == 0)
            status.set(.negative, to: registers.x & 0x80)

        case .txs:
            stackPointer = registers.x

        case .adc(let mode):
            let value = operand(for: mode)
            let carry = status.contains(.carry) ? 1 : 0
            let sum = UInt16(registers.a) + UInt16(value) + UInt16(carry)
            let result = UInt8(truncatingIfNeeded: sum)
            status.set(.carry, to: sum > 0xFF)
            status.set(.overflow, to: ~(registers.a ^ value) & (registers.a ^ result) & 0x80)
            status.set(.zero, to: result == 0)
            status.set(.negative, to: result & 0x80)
            registers.a = result

        case .sbc(let mode):
            let value = operand(for: mode)
            let carry = status.contains(.carry) ? 1 : 0
            let sum = UInt16(registers.a) + UInt16(~value) + UInt16(carry)
            let result = UInt8(truncatingIfNeeded: sum)
            status.set(.carry, to: sum > 0xFF)
            status.set(.overflow, to: (registers.a ^ result) & (registers.a ^ value) & 0x80)
            status.set(.zero, to: result == 0)
            status.set(.negative, to: result & 0x80)
            registers.a = result

        case .inc(let mode):
            let result = operand(for: mode) &+ 1
            bus.write(result, at: address(for: mode))
            status.set(.zero, to: result == 0)
            status.set(.negative, to: result & 0x80)

        case .dec(let mode):
            let result = operand(for: mode) &- 1
            bus.write(result, at: address(for: mode))
            status.set(.zero, to: result == 0)
            status.set(.negative, to: result & 0x80)

        case .inx:
            registers.x &+= 1
            status.set(.zero, to: registers.x == 0)
            status.set(.negative, to: registers.x & 0x80)

        case .iny:
            registers.y &+= 1
            status.set(.zero, to: registers.y == 0)
            status.set(.negative, to: registers.y & 0x80)

        case .dex:
            registers.x &-= 1
            status.set(.zero, to: registers.x == 0)
            status.set(.negative, to: registers.x & 0x80)

        case .dey:
            registers.y &-= 1
            status.set(.zero, to: registers.y == 0)
            status.set(.negative, to: registers.y & 0x80)

        case .asl(let mode):
            let value = operand(for: mode)
            let result = value << 1
            status.set(.carry, to: value & 0x80)
            status.set(.zero, to: result == 0)
            status.set(.negative, to: result & 0x80)
            updateOperand(for: mode, with: result)

        case .lsr(let mode):
            let value = operand(for: mode)
            let result = value >> 1
            status.set(.carry, to: value & 1)
            status.set(.zero, to: result == 0)
            status.set(.negative, to: result & 0x80)
            updateOperand(for: mode, with: result)

        case .rol(let mode):
            let value = operand(for: mode)
            let result = (value << 1) | (status.contains(.carry) ? 1 : 0)
            status.set(.carry, to: value & 0x80)
            status.set(.zero, to: result == 0)
            status.set(.negative, to: result & 0x80)
            updateOperand(for: mode, with: result)

        case .ror(let mode):
            let value = operand(for: mode)
            let result = (value >> 1) | (status.contains(.carry) ? 0x80 : 0)
            status.set(.carry, to: value & 1)
            status.set(.zero, to: result == 0)
            status.set(.negative, to: result & 0x80)
            updateOperand(for: mode, with: result)

        case .and(let mode):
            registers.a &= operand(for: mode)
            status.set(.zero, to: registers.a == 0)
            status.set(.negative, to: registers.a & 0x80)

        case .ora(let mode):
            registers.a |= operand(for: mode)
            status.set(.zero, to: registers.a == 0)
            status.set(.negative, to: registers.a & 0x80)

        case .eor(let mode):
            registers.a ^= operand(for: mode)
            status.set(.zero, to: registers.a == 0)
            status.set(.negative, to: registers.a & 0x80)

        case .bit(let mode):
            let value = operand(for: mode)
            let result = registers.a & value
            status.set(.zero, to: result == 0)
            status.set(.overflow, to: value & 0x40)
            status.set(.negative, to: value & 0x80)

        case .cmp(let mode):
            let value = operand(for: mode)
            let (result, overflow) = registers.a.subtractingReportingOverflow(value)
            status.set(.carry, to: !overflow)
            status.set(.zero, to: result == 0)
            status.set(.negative, to: result & 0x80)

        case .cpx(let mode):
            let value = operand(for: mode)
            let (result, overflow) = registers.x.subtractingReportingOverflow(value)
            status.set(.carry, to: !overflow)
            status.set(.zero, to: result == 0)
            status.set(.negative, to: result & 0x80)

        case .cpy(let mode):
            let value = operand(for: mode)
            let (result, overflow) = registers.y.subtractingReportingOverflow(value)
            status.set(.carry, to: !overflow)
            status.set(.zero, to: result == 0)
            status.set(.negative, to: result & 0x80)

        case .bcc(let mode):
            if !status.contains(.carry) {
                branch(to: address(for: mode))
            }

        case .bcs(let mode):
            if status.contains(.carry) {
                branch(to: address(for: mode))
            }

        case .beq(let mode):
            if status.contains(.zero) {
                branch(to: address(for: mode))
            }

        case .bmi(let mode):
            if status.contains(.negative) {
                branch(to: address(for: mode))
            }

        case .bne(let mode):
            if !status.contains(.zero) {
                branch(to: address(for: mode))
            }

        case .bpl(let mode):
            if !status.contains(.negative) {
                branch(to: address(for: mode))
            }

        case .bvc(let mode):
            if !status.contains(.overflow) {
                branch(to: address(for: mode))
            }

        case .bvs(let mode):
            if status.contains(.overflow) {
                branch(to: address(for: mode))
            }

        case .jmp(let mode):
            programCounter = address(for: mode)

        case .jsr(let mode):
            pushStackWord(value: programCounter)
            programCounter = address(for: mode)

        case .rts:
            programCounter = pullStackWord()

        case .brk:
            pushStackWord(value: programCounter &+ 2)
            pushStackByte(status.rawValue)
            programCounter = bus.readWord(at: 0xFFFE)
            status.insert([.interruptDisable, .breakCommand])

        case .rti:
            status = StatusFlags(rawValue: pullStackByte())
            programCounter = pullStackWord()

        case .pha:
            pushStackByte(registers.a)

        case .php:
            pushStackByte(status.rawValue)

        case .pla:
            registers.a = pullStackByte()

        case .plp:
            status = StatusFlags(rawValue: pullStackByte())
            status.insert(.unused)

        case .clc:
            status.remove(.carry)

        case .cld:
            status.remove(.decimal)

        case .cli:
            status.remove(.interruptDisable)

        case .clv:
            status.remove(.overflow)

        case .sec:
            status.insert(.carry)

        case .sed:
            status.insert(.decimal)

        case .sei:
            status.insert(.interruptDisable)

        case .nop:
            break
        }
    }

    func clock() {
        print("CPU: Remaining cycles: \(remainingCycles)")

        if remainingCycles == 0 {
            let opcode = fetchByte()
            let (instruction, cycles) = decode(opcode)
            print("CPU: Instruction: \(instruction), Cycles: \(cycles)")
            execute(instruction)
            remainingCycles += cycles
        }

        remainingCycles -= 1
    }
}
