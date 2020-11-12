#pragma once

#include <QtWidgets/QMainWindow>
#include "ui_MonteCarlo_gui_qt.h"

class MonteCarlo_gui_qt : public QMainWindow
{
    Q_OBJECT

public:
    MonteCarlo_gui_qt(QWidget *parent = Q_NULLPTR);

private:
    Ui::MonteCarlo_gui_qtClass ui;
};
