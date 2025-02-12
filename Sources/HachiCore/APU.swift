class APU {
    private var registers: [UInt8] = Array(repeating: 0, count: 0x16)

    func read(at address: UInt16) -> UInt8 {
        let register = Int(address - 0x4000)
        guard register < registers.count else { return 0 }
        return registers[register]
    }

    func write(_ value: UInt8, at address: UInt16) {
        let register = Int(address - 0x4000)
        guard register < registers.count else { return }
        registers[register] = value
    }
}
