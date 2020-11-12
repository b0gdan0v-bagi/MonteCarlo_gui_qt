#include "MonteCarlo_gui_qt.h"
#include <QtWidgets/QApplication>

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    MonteCarlo_gui_qt w;
    w.show();
    return a.exec();
}
