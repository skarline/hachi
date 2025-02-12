import Foundation

enum CartridgeError: Error {
    case invalidHeader
    case invalidMagic
    case incompleteROM
    case unknownFormat
}

class Cartridge {
    let prgROM: Data
    let chrROM: Data
    let mapper: Int

    init(data: Data) throws {
        guard data.count >= 16 else {
            throw CartridgeError.invalidHeader
        }

        let header = data.subdata(in: 0..<16)
        guard
            header[0] == 0x4E,
            header[1] == 0x45,
            header[2] == 0x53,
            header[3] == 0x1A
        else {
            throw CartridgeError.invalidMagic
        }

        let hasTrainer = (header[6] & 0x04) != 0

        let offset = 16 + (hasTrainer ? 512 : 0)

        let prgUnits = Int(header[4])
        let chrUnits = Int(header[5])
        let prgBytes = prgUnits * 16384
        let chrBytes = chrUnits * 8192

        let mapperNumber = (Int(header[7]) & 0xF0) | (Int(header[6]) >> 4)

        guard data.count >= offset + prgBytes + chrBytes else {
            throw CartridgeError.incompleteROM
        }

        self.prgROM = data.subdata(in: offset..<offset + prgBytes)
        self.chrROM = data.subdata(in: offset + prgBytes..<offset + prgBytes + chrBytes)
        self.mapper = mapperNumber
    }

    func read(at address: UInt16) -> UInt8 {
        switch address {
        case 0x8000...0xFFFF:
            let prgSize = prgROM.count
            let prgAddress = Int(address - 0x8000)

            let mirroredAddress = prgSize == 0x4000 ? prgAddress & 0x3FFF : prgAddress
            guard mirroredAddress < prgSize else { return 0 }
            return prgROM[mirroredAddress]

        case 0x0000...0x1FFF:
            let chrAddress = Int(address)
            guard chrAddress < chrROM.count else { return 0 }
            return chrROM[chrAddress]

        default:
            return 0
        }
    }

    func write(_ value: UInt8, at address: UInt16) {}
}
