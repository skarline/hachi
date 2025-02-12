class Bus {
    private var ram: [UInt8] = Array(repeating: 0, count: 2048)
    private var interruptVectors: [UInt8] = Array(repeating: 0, count: 6)

    private var ppu: PPU?
    private var apu: APU?
    private var cartridge: Cartridge?

    func connect(ppu: PPU) {
        self.ppu = ppu
    }

    func connect(apu: APU) {
        self.apu = apu
    }

    func connect(cartridge: Cartridge) {
        self.cartridge = cartridge
    }

    func read<T: BinaryInteger>(at address: T) -> UInt8 {
        let address = UInt16(address)
        switch address {
        case 0x0000..<0x2000:
            return ram[Int(address & 0x07FF)]

        case 0x2000..<0x4000:
            return ppu?.read(at: address) ?? 0

        case 0x4000..<0x4016:
            return apu?.read(at: address) ?? 0

        case 0x4016:
            return 0

        case 0x4017:
            return 0

        case 0x4018..<0x8000:
            return 0

        case 0xFFFA...0xFFFF:
            return interruptVectors[Int(address - 0xFFFA)]

        case 0x8000...0xFFFF:
            return cartridge?.read(at: address) ?? 0

        default:
            return 0
        }
    }

    func write(_ value: UInt8, at address: UInt16) {
        switch address {
        case 0x0000..<0x2000:
            ram[Int(address & 0x07FF)] = value

        case 0x2000..<0x4000:
            ppu?.write(value, at: address)

        case 0x4000..<0x4016:
            apu?.write(value, at: address)

        case 0x4016:
            break

        case 0x4017:
            break

        case 0x4018..<0x8000:
            break

        case 0xFFFA...0xFFFF:
            interruptVectors[Int(address - 0xFFFA)] = value

        case 0x8000...0xFFFF:
            cartridge?.write(value, at: address)

        default:
            break
        }
    }

    func readWord<T: BinaryInteger>(at address: T) -> UInt16 {
        let lo = read(at: address)
        let hi = read(at: address + 1)
        return UInt16(hi) << 8 | UInt16(lo)
    }

    func writeWord(_ value: UInt16, at address: UInt16) {
        let lo = UInt8(truncatingIfNeeded: value)
        let hi = UInt8(value >> 8)
        write(lo, at: address)
        write(hi, at: address + 1)
    }
}
