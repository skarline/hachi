import Foundation

enum MapperAccess {
    case cpu(UInt16)
    case ppu(UInt16)
}

protocol Mapper {
    func read(_ access: MapperAccess) -> UInt8
    func write(_ access: MapperAccess, _ value: UInt8)
}

class Mapper000: Mapper {
    let prg: Data
    let chr: Data

    init(prg: Data, chr: Data) {
        self.prg = prg
        self.chr = chr
    }

    func read(_ access: MapperAccess) -> UInt8 {
        switch access {
        case .cpu(let address):
            if (0x8000...0xFFFF).contains(address) {
                let prgSize = prg.count
                let prgAddress = Int(address - 0x8000)

                if prgSize == 0x4000 {
                    return prg[prgAddress & 0x3FFF]
                }
                return prg[prgAddress]
            }
            return 0
        case .ppu(let address):
            if address < 0x2000, Int(address) < chr.count {
                return chr[Int(address)]
            }
            return 0
        }
    }

    func write(_ access: MapperAccess, _ value: UInt8) {}
}
