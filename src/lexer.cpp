#include "lexer.hpp"
#include <cstdlib>

namespace mycc {

static TokenKind keywordKind(const std::string& s) {
    static const std::unordered_map<std::string, TokenKind> kw = {
        {"funcao", TokenKind::KwFuncao},
        {"variavel", TokenKind::KwVariavel},
        {"var", TokenKind::KwVariavel},
        {"retorna", TokenKind::KwRetorna},
        {"retorne", TokenKind::KwRetorna},
        {"se", TokenKind::KwSe},
        {"senao", TokenKind::KwSenao},
        {"enquanto", TokenKind::KwEnquanto},
        {"inteiro", TokenKind::KwInteiro},
        {"logico", TokenKind::KwLogico},
        {"vazio", TokenKind::KwVazio},
        {"verdadeiro", TokenKind::KwVerdadeiro},
        {"falso", TokenKind::KwFalso},
    };
    auto it = kw.find(s);
    return it == kw.end() ? TokenKind::Identifier : it->second;
}

void Lexer::skipSpacesAndComments() {
    while (true) {
        char c = peek();
        if (c == '\0') return;
        if (c == ' ' || c == '\t' || c == '\r' || c == '\n') { get(); continue; }
        // Comentários // até fim da linha
        if (c == '/' && i + 1 < text.size() && text[i+1] == '/') {
            while (peek() != '\n' && peek() != '\0') get();
            continue;
        }
        break;
    }
}

std::vector<Token> Lexer::tokenize() {
    std::vector<Token> toks;
    while (true) {
        skipSpacesAndComments();
        int l = line, c = col;
        char ch = peek();
        if (ch == '\0') {
            toks.push_back(makeToken(TokenKind::End, "", l, c));
            break;
        }

        // Ident/kw
        if (std::isalpha((unsigned char)ch) || ch == '_') {
            std::string s;
            while (std::isalnum((unsigned char)peek()) || peek() == '_') s.push_back(get());
            TokenKind k = keywordKind(s);
            toks.push_back(makeToken(k, s, l, c));
            continue;
        }
        // Número inteiro
        if (std::isdigit((unsigned char)ch)) {
            std::string n;
            while (std::isdigit((unsigned char)peek())) n.push_back(get());
            toks.push_back(makeToken(TokenKind::IntLiteral, n, l, c));
            continue;
        }
        // Símbolos/operadores
        switch (ch) {
            case '(': get(); toks.push_back(makeToken(TokenKind::LParen, "(", l, c)); continue;
            case ')': get(); toks.push_back(makeToken(TokenKind::RParen, ")", l, c)); continue;
            case '{': get(); toks.push_back(makeToken(TokenKind::LBrace, "{", l, c)); continue;
            case '}': get(); toks.push_back(makeToken(TokenKind::RBrace, "}", l, c)); continue;
            case ',': get(); toks.push_back(makeToken(TokenKind::Comma, ",", l, c)); continue;
            case ':': get(); toks.push_back(makeToken(TokenKind::Colon, ":", l, c)); continue;
            case ';': get(); toks.push_back(makeToken(TokenKind::Semicolon, ";", l, c)); continue;
            case '[': get(); toks.push_back(makeToken(TokenKind::LBracket, "[", l, c)); continue;
            case ']': get(); toks.push_back(makeToken(TokenKind::RBracket, "]", l, c)); continue;
            case '+': get(); toks.push_back(makeToken(TokenKind::Plus, "+", l, c)); continue;
            case '-': get(); toks.push_back(makeToken(TokenKind::Minus, "-", l, c)); continue;
            case '*': get(); toks.push_back(makeToken(TokenKind::Star, "*", l, c)); continue;
            case '%': get(); toks.push_back(makeToken(TokenKind::Percent, "%", l, c)); continue;
            case '!': {
                get();
                if (peek() == '=') { get(); toks.push_back(makeToken(TokenKind::BangEq, "!=", l, c)); }
                else { toks.push_back(makeToken(TokenKind::Bang, "!", l, c)); }
                continue;
            }
            case '=': {
                get();
                if (peek() == '=') { get(); toks.push_back(makeToken(TokenKind::EqEq, "==", l, c)); }
                else { toks.push_back(makeToken(TokenKind::Assign, "=", l, c)); }
                continue;
            }
            case '<': {
                get();
                if (peek() == '=') { get(); toks.push_back(makeToken(TokenKind::Le, "<=", l, c)); }
                else { toks.push_back(makeToken(TokenKind::Lt, "<", l, c)); }
                continue;
            }
            case '>': {
                get();
                if (peek() == '=') { get(); toks.push_back(makeToken(TokenKind::Ge, ">=", l, c)); }
                else { toks.push_back(makeToken(TokenKind::Gt, ">", l, c)); }
                continue;
            }
            case '/': {
                // já tratamos '//' em skip; se sobrou '/', é operador divisão
                get(); toks.push_back(makeToken(TokenKind::Slash, "/", l, c)); continue;
            }
        }

        // caractere inesperado
        diag.error(l, c, std::string("caractere inesperado: '") + ch + "'");
        get(); // consome para não travar
    }
    return toks;
}

} // namespace mycc