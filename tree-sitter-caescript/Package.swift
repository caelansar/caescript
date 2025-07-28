// swift-tools-version:5.3
import PackageDescription

let package = Package(
    name: "TreeSitterCaescript",
    products: [
        .library(name: "TreeSitterCaescript", targets: ["TreeSitterCaescript"]),
    ],
    dependencies: [
        .package(url: "https://github.com/ChimeHQ/SwiftTreeSitter", from: "0.8.0"),
    ],
    targets: [
        .target(
            name: "TreeSitterCaescript",
            dependencies: [],
            path: ".",
            sources: [
                "src/parser.c",
                // NOTE: if your language has an external scanner, add it here.
            ],
            resources: [
                .copy("queries")
            ],
            publicHeadersPath: "bindings/swift",
            cSettings: [.headerSearchPath("src")]
        ),
        .testTarget(
            name: "TreeSitterCaescriptTests",
            dependencies: [
                "SwiftTreeSitter",
                "TreeSitterCaescript",
            ],
            path: "bindings/swift/TreeSitterCaescriptTests"
        )
    ],
    cLanguageStandard: .c11
)
