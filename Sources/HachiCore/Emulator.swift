import Foundation

public class Emulator {
    public let bus: Bus
    public let cpu: CPU
    public let ppu: PPU
    public let apu: APU

    public init() {
        let bus = Bus()

        self.bus = bus
        self.cpu = CPU(bus: bus)
        self.ppu = PPU(bus: bus)
        self.apu = APU(bus: bus)
    }

    public func loadROMFile(path: String) throws {
        let data = try Data(contentsOf: URL(fileURLWithPath: path))
        let cartridge = try Cartridge(data: data)
        self.bus.connect(cartridge: cartridge)
    }
}
