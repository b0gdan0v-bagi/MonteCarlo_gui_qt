#pragma once

#include <QMainWindow>
#include <QApplication>
#include <thread>
#include "listwidget.h"

QT_BEGIN_NAMESPACE
class QAction;
class QMenu;
class QPlainTextEdit;
class QSessionManager;
QT_END_NAMESPACE

class MainFrame : public QMainWindow {

    Q_OBJECT

public:
    MainFrame(QWidget* parent = 0);
    ListWidget* lw;

private slots:

    void about();
    void aboutMC();
    bool loadConfig();
    bool saveConfig();
private:

    bool maybeSave();
    unsigned int cores = std::thread::hardware_concurrency();
};