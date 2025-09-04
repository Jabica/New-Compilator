cmake_minimum_required(VERSION 3.16)
project(mycc-pt VERSION 0.1 LANGUAGES C CXX)

# Evita os warnings de "newer macOS version"
# (coloque a versão do seu sistema; pelos logs parece 14.4)
set(CMAKE_OSX_DEPLOYMENT_TARGET "14.4")

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# LLVM
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# Inclui diretórios do LLVM como SYSTEM para reduzir warnings
include_directories(SYSTEM ${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# === BIBLIOTECA mycc ===
# Liste apenas .cpp aqui. Headers ficam expostos via include/.
add_library(mycc STATIC
    src/cli.cpp
    src/lexer.cpp
    src/parser.cpp
    src/codegen.cpp
    # se houver .cpp de ast/diagnostics/token, adicione-os aqui:
    # src/ast.cpp
    # src/diagnostics.cpp
    # src/token.cpp
)

# Exponha os headers públicos a quem linkar com a lib
# (coloque TODOS os .hpp públicos em include/, não em src/)
target_include_directories(mycc
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}/include
    PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/src
)

# Escolha os componentes do LLVM de que você precisa
# (pelo seu projeto, core e support costumam bastar)
llvm_map_components_to_libnames(LLVM_LIBS core support)

# Linke LLVM na biblioteca (se a lib usa LLVM diretamente)
target_link_libraries(mycc PRIVATE ${LLVM_LIBS})

# === EXECUTÁVEL ===
add_executable(mycc_cli src/main.cpp)
target_link_libraries(mycc_cli PRIVATE mycc ${LLVM_LIBS})

# Instalação
install(TARGETS mycc mycc_cli
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib)