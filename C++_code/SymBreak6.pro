TEMPLATE = app
CONFIG += console c++17
CONFIG -= app_bundle
CONFIG -= qt

SOURCES += \
        Cell.cpp \
        Individual.cpp \
        Population.cpp \
        main.cpp \
        randomnumbers.cpp


## use Intel TBB for parallelization
## for how to install TBB and latest g++:
## https://solarianprogrammer.com/2019/05/09/cpp-17-stl-parallel-algorithms-gcc-intel-tbb-linux-macos/
INCLUDEPATH += /usr/local/tbb-2019_U9/lib

LIBS += -L/usr/local/tbb-2019_U9/lib
LIBS += -ltbb
## /use Intel TBB

HEADERS += \
    Cell.h \
    Individual.h \
    Population.h \
    parameter_values.h \
    randomnumbers.h \
    utils.h
