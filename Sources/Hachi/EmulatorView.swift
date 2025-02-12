import HachiCore
import SwiftUI

struct EmulatorView: View {
    @State private var isTargeted = false

    var body: some View {
        ZStack {
            Color.black

            if isTargeted {
                Rectangle().fill(Color.accentColor.opacity(0.2))
            }
        }
        .onDrop(of: [.fileURL], isTargeted: $isTargeted) { providers in
            guard let provider = providers.first else { return false }

            _ = provider.loadObject(ofClass: URL.self) { url, error in
                guard error == nil,
                    let url = url,
                    url.pathExtension.lowercased() == "nes"
                else {
                    return
                }

                DispatchQueue.main.async {
                    self.loadROM(path: url.path)
                }
            }

            return true
        }
        .frame(minWidth: 512, minHeight: 480)
    }

    func loadROM(path: String) {
        print(path)
    }
}
