#include "mainframe.h"
#include <QToolBar>
#include <QIcon>
#include <QAction>
#include <QMenu>
#include <QMenuBar>
#include <QStatusBar>
#include <QTextEdit>
#include <QVBoxLayout>
#include <QMessagebox>

MainFrame::MainFrame(QWidget* parent)
    : QMainWindow(parent) {

    lw = new ListWidget(this);

    QPixmap newpix("new.png");
    QPixmap openpix("open.png");
    QPixmap quitpix("quit.png");

    QAction* quit = new QAction("&Quit", this);

    QMenu* file;
    file = menuBar()->addMenu("&File");
    QAction* saveConfig = file->addAction(tr("&Save"),this, &MainFrame::saveConfig);
    QAction* loadConfig = file->addAction(tr("&Load"), this, &MainFrame::loadConfig);
    file->addSeparator();
    file->addAction(quit);

    connect(quit, &QAction::triggered, qApp, &QApplication::quit);

    QMenu* help;
    help = menuBar()->addMenu("&About");
    QAction* aboutAct = help->addAction(tr("&About"), this, &MainFrame::about);
    QAction* aboutActMC = help->addAction(tr("&About MC.exe"), this, &MainFrame::aboutMC);
    aboutAct->setStatusTip(tr("About box"));

    

    QToolBar* toolbar = addToolBar("main toolbar");
    toolbar->addAction(saveConfig);
    toolbar->addAction(loadConfig);
    toolbar->addSeparator();

    QAction* quit2 = toolbar->addAction(QIcon(quitpix), "Quit Application");
    connect(quit2, &QAction::triggered, qApp, &QApplication::quit);

    setCentralWidget(lw);

    statusBar()->showMessage("Ready");
}

void MainFrame::about()
{
    QString text = "The Monte - Carlo GUI applicaton runs simulations(recommended up to " + QString::number(cores) +
        " for your system) of MC.exe with different parameters\nWritten by Andrey Bogdanov, 2020";
    QMessageBox::about(this, tr("About MC gui"), text);
}

void MainFrame::aboutMC()
{
    QString text = "This program use Monte-Carlo methods for calculating electron transport in narrow gap semiconductors"
        ", quantum wells e.t.c and studying impact ionization\nCompiled with PGI Fortran compilier, 2018-2020";
    QMessageBox::about(this, tr("About MC.exe"), text);
}
// think how to do it with connect?
bool MainFrame::saveConfig()
{
    lw->saveConfig();
    return true;
}
bool MainFrame::maybeSave()
{
    const QMessageBox::StandardButton ret
        = QMessageBox::warning(this, tr("Save?"),
            tr("Do you want to save your changes?"),
            QMessageBox::Save | QMessageBox::Discard | QMessageBox::Cancel);
    switch (ret) {
    case QMessageBox::Save:
        return saveConfig();
    case QMessageBox::Cancel:
        return false;
    default:
        break;
    }
    return true;
}
bool MainFrame::loadConfig()
{
    lw->loadConfig();
    return true;
}
