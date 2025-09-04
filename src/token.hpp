#pragma once
#include <string>
#include <string_view>

namespace mycc {

enum class TokenKind {
    End,            // EOF
    Identifier,
    IntLiteral,
    // Palavras-chave
    KwFuncao, KwVariavel, KwRetorna, KwSe, KwSenao, KwEnquanto,
    KwInteiro, KwLogico, KwVazio, KwVerdadeiro, KwFalso,
    // Símbolos
    LParen, RParen, LBrace, RBrace, Comma, Colon, Semicolon,
    LBracket, RBracket,
    Assign, // =
    // Operadores
    Plus, Minus, Star, Slash, Percent,
    Bang, // !
    EqEq, BangEq,
    Lt, Gt, Le, Ge,
};

struct Token {
    TokenKind kind;
    std::string lexeme;
    int line = 1;
    int col = 1;
};

inline const char* to_string(TokenKind k) {
    switch (k) {
        case TokenKind::End: return "End";
        case TokenKind::Identifier: return "Identifier";
        case TokenKind::IntLiteral: return "IntLiteral";
        case TokenKind::KwFuncao: return "funcao";
        case TokenKind::KwVariavel: return "variavel";
        case TokenKind::KwRetorna: return "retorna";
        case TokenKind::KwSe: return "se";
        case TokenKind::KwSenao: return "senao";
        case TokenKind::KwEnquanto: return "enquanto";
        case TokenKind::KwInteiro: return "inteiro";
        case TokenKind::KwLogico: return "logico";
        case TokenKind::KwVazio: return "vazio";
        case TokenKind::KwVerdadeiro: return "verdadeiro";
        case TokenKind::KwFalso: return "falso";
        case TokenKind::LParen: return "(";
        case TokenKind::RParen: return ")";
        case TokenKind::LBrace: return "{";
        case TokenKind::RBrace: return "}";
        case TokenKind::Comma: return ",";
        case TokenKind::Colon: return ":";
        case TokenKind::Semicolon: return ";";
        case TokenKind::LBracket: return "[";
        case TokenKind::RBracket: return "]";
        case TokenKind::Assign: return "=";
        case TokenKind::Plus: return "+";
        case TokenKind::Minus: return "-";
        case TokenKind::Star: return "*";
        case TokenKind::Slash: return "/";
        case TokenKind::Percent: return "%";
        case TokenKind::Bang: return "!";
        case TokenKind::EqEq: return "==";
        case TokenKind::BangEq: return "!=";
        case TokenKind::Lt: return "<";
        case TokenKind::Gt: return ">";
        case TokenKind::Le: return "<=";
        case TokenKind::Ge: return ">=";
    }
    return "?";
}

} // namespace mycc