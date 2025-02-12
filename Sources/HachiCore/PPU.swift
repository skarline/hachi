class PPU {
    // Memory-mapped registers
    private var registers: [UInt8] = Array(repeating: 0, count: 8)

    func read(at address: UInt16) -> UInt8 {
        // PPU registers are mapped to 0x2000-0x2007 and mirrored every 8 bytes until 0x3FFF
        let register = Int(address & 0x7)  // Get register index (0-7)
        return registers[register]
    }

    func write(_ value: UInt8, at address: UInt16) {
        // PPU registers are mapped to 0x2000-0x2007 and mirrored every 8 bytes until 0x3FFF
        let register = Int(address & 0x7)  // Get register index (0-7)
        registers[register] = value
    }
}
