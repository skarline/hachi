import Foundation

enum CartridgeError: Error {
    case invalidHeader
    case invalidMagic
    case invalidMapper
    case incompleteROM
    case unknownFormat
}

class Cartridge {
    private let prg: Data
    private let chr: Data

    let mapper: Mapper

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

        self.prg = data.subdata(in: offset..<offset + prgBytes)
        self.chr = data.subdata(in: offset + prgBytes..<offset + prgBytes + chrBytes)

        switch mapperNumber {
        case 0:
            self.mapper = Mapper000(prg: self.prg, chr: self.chr)
        default:
            throw CartridgeError.invalidMapper
        }
    }
}
