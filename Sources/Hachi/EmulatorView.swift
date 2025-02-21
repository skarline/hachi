import HachiCore
import SwiftUI
import UniformTypeIdentifiers

struct EmulatorView: View {
    @State private var isTargeted = false
    @State private var isRunning = false
    @State private var emulationTask: Task<Void, Never>?
    @State private var emulator: Emulator?

    var fileType: UTType {
        UTType(filenameExtension: "nes")!
    }

    var body: some View {
        ZStack {
            Color.black

            if isTargeted {
                Rectangle().fill(Color.accentColor.opacity(0.2))
            }
        }
        .dropDestination(for: URL.self) { items, location in
            guard let url = items.first else { return false }

            do {
                try load(path: url.path)
            } catch {
                return false
            }

            return true
        } isTargeted: { isTargeted in
            self.isTargeted = isTargeted
        }
        .frame(minWidth: 512, minHeight: 480)
        .onDisappear {
            stopEmulation()
        }
    }

    private func load(path: String) throws {
        let emulator = Emulator()
        try emulator.loadROMFile(path: path)
        self.emulator = emulator
        startEmulation()
    }

    private func startEmulation() {
        isRunning = true
        emulationTask?.cancel()
        emulationTask = Task(priority: .userInitiated) { [weak emulator] in
            while !Task.isCancelled && isRunning {
                emulator?.bus.cycle()
            }
        }
    }

    private func stopEmulation() {
        isRunning = false
        emulationTask?.cancel()
        emulationTask = nil
    }
}
