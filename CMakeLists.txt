cmake_minimum_required(VERSION 3.10)

project(ces41lex VERSION 0.1 LANGUAGES C)

include(CMakePrintHelpers)
cmake_print_variables(CMAKE_VERSION)

# https://cmake.org/cmake/help/latest/module/FindFLEX.html
find_package(BISON)
find_package(FLEX)

SET(CES41_SRC "src" CACHE FILEPATH "Directory with student sources")

include_directories( ${CES41_SRC} include lib)

FILE(GLOB labSrc ${CES41_SRC}/*.c  )
FILE(GLOB lablib lib/*.c  )

#file(GLOB_RECURSE C_FILES ${CES41_SRC}/*.c)
#set_source_files_properties(${C_FILES} {CMAKE_CURRENT_BINARY_DIR}/lexer.c )

FLEX_TARGET(scanner ${CES41_SRC}/cminus.l  ${CMAKE_CURRENT_BINARY_DIR}/lexer.c )
BISON_TARGET(myparser ${CES41_SRC}/cminus.y ${CMAKE_CURRENT_BINARY_DIR}/parser.c)
ADD_FLEX_BISON_DEPENDENCY(scanner myparser)

message("   * Flex OUT = ${FLEX_scanner_OUTPUTS}")
message("   * BisonOUT = ${BISON_myparser_OUTPUTS}")
message("   * SRC FILES = ${labSrc}")

include_directories(${CMAKE_CURRENT_BINARY_DIR})
 add_executable(mylex
    ${labSrc}
    ${lablib}
    ${BISON_myparser_OUTPUTS}
    ${FLEX_scanner_OUTPUTS}
 )
 target_link_libraries(mylex )
 #${FLEX_LIBRARIES})
 # compilation problem of undefined yylex - noyywrap - it only works with one src file
 # https://stackoverflow.com/questions/1480138/undefined-reference-to-yylex

add_custom_target(runmylex ALL
  COMMENT "running mylex"
  COMMAND ../scripts/runlex
  DEPENDS mylex
  VERBATIM
  USES_TERMINAL
)

add_custom_target(rundiff ALL
  COMMENT "running diff"
  COMMAND ../scripts/rundiff
  DEPENDS runmylex
  VERBATIM
  USES_TERMINAL
)

add_custom_target(ddiff 
  COMMENT "running diff"
  COMMAND ../scripts/rundetaildiff
  DEPENDS runmylex
  VERBATIM
  USES_TERMINAL
)


# explaning about diff options
# diff -Z (trail space) -b (change space) -B (blank lines)

#######################################33
### previous attempts, less succesfull
# works, but file is hardcoded
# add_custom_target(mdcout ALL
#   COMMAND mylex ../example/mdc.cm > mdc.out
#   COMMENT "running diff for mdc.cm"
#   COMMAND diff -ZbB ../output/mdc.out  mdc.out
#   DEPENDS mylex
# )

# problem: does not run when lex is up to date
#add_custom_command( TARGET lex POST_BUILD
#  COMMAND lex ../example/mdc.cm > mdc.out
#  COMMENT "running diff for mdc.cm"
#  COMMAND diff -ZbB ../output/mdc.out  mdc.out
#  MAIN_DEPENDENCY lex
#)
##########################################3


