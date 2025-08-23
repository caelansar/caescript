import XCTest
import SwiftTreeSitter
import TreeSitterCaescript

final class TreeSitterCaescriptTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_caescript())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Caescript grammar")
    }
}
