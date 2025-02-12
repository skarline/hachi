// swift-tools-version: 6.0
import PackageDescription

let package = Package(
    name: "Hachi",
    platforms: [
        .macOS(.v14)
    ],
    targets: [
        .executableTarget(
            name: "Hachi",
            dependencies: ["HachiCore"]
        ),
        .target(
            name: "HachiCore"
        ),
        .testTarget(
            name: "HachiTests",
            dependencies: ["Hachi"]
        ),
    ]
)
