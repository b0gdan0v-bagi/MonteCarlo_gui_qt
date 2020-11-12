#include "listwidget.h"
#include <QVBoxLayout>
#include <QInputDialog>
#include <qmessagebox.h>
#include <QMenuBar>
#include <QMenu>
#include <QProcess>
#include <QDir>

#include <windows.h>
#include <ShellApi.h>

#include <QCheckBox>
#include <QComboBox>
#include <QHBoxLayout>
#include <QLabel>
#include <QSpinBox>
#include <QStackedWidget>

#include <QList>

ListWidget::ListWidget(QWidget* parent)
    : QWidget(parent) {
    
    QVBoxLayout* vbox = new QVBoxLayout();
    vbox->setSpacing(10);
    //QVBoxLayout* vbox2 = new QVBoxLayout();
    //vbox2->setSpacing(10);
    QGridLayout* controlsLayout = new QGridLayout;

    QHBoxLayout* hbox = new QHBoxLayout(this);

    lw = new QListWidget(this);
    //lw->addItem("si");
    //lw->addItem("inas");


    save = new QPushButton("Save", this);
    add = new QPushButton("Add", this);
    rename = new QPushButton("Rename", this);
    remove = new QPushButton("Remove", this);
    removeAll = new QPushButton("Remove All", this);
    testButton = new QPushButton("TEST", this);
    
    // material;

    materialLabel = new QLabel("Material :");
    materialCB = new QComboBox(this);
    materialCB->addItem("ge");
    materialCB->addItem("si");
    materialCB->addItem("inas");
    materialCB->addItem("ingaas");
    materialCB->addItem("cdhgte");
    controlsLayout->addWidget(materialLabel, 0, 0);
    controlsLayout->addWidget(materialCB,0,1);

    // iterations

    iterationsSB = new QSpinBox(this);
    iterationsSB->setMinimum(3);
    iterationsSB->setMaximum(7);
    iterationsSB->setValue(4);
    iterationsLabel = new QLabel("iterations = 10^");
    controlsLayout->addWidget(iterationsLabel, 1, 0);
    controlsLayout->addWidget(iterationsSB, 1, 1);

    // max field

    maxFieldLabel = new QLabel("max field, V/sm");
    maxFieldLE = new QLineEdit(this);
    maxFieldLE->setValidator(new QIntValidator(1, 1000000, this));
    maxFieldLE->setText(QString::number(1200));
    numberFieldsToCalcLabel = new QLabel("fields to calculate");
    numberFieldsToCalcSB = new QSpinBox(this);
    numberFieldsToCalcSB->setMinimum(2);
    numberFieldsToCalcSB->setMaximum(10);
    numberFieldsToCalcSB->setValue(3);
    controlsLayout->addWidget(maxFieldLabel, 2, 0);
    controlsLayout->addWidget(maxFieldLE, 2, 1);
    controlsLayout->addWidget(numberFieldsToCalcLabel, 2, 2);
    controlsLayout->addWidget(numberFieldsToCalcSB, 2, 3);

    // out points

    outputPointsLabel = new QLabel("Number of output points ");
    outputPountsLE = new QLineEdit(this);
    outputPountsLE->setValidator(new QIntValidator(1, 50000, this));
    outputPountsLE->setText(QString::number(40000));
    controlsLayout->addWidget(outputPointsLabel, 3, 0);
    controlsLayout->addWidget(outputPountsLE, 3, 1);

    // gamma

    gammaLabel = new QLabel("gamma = 10^");
    gammaSB = new QSpinBox(this);
    gammaSB->setMinimum(8);
    gammaSB->setMaximum(15);
    gammaSB->setValue(12);
    controlsLayout->addWidget(gammaLabel, 4, 0);
    controlsLayout->addWidget(gammaSB, 4, 1);

    // temperature

    temperatureLabel = new QLabel(tr("Temperature, K"));
    temperatureLE = new QLineEdit(this);
    temperatureLE->setValidator(new QIntValidator(0,400,this));
    temperatureLE->setText(QString::number(77));
    roomT = new QPushButton("room", this);
    nitrogenT = new QPushButton("nitro", this);
    heliumT = new QPushButton("hellium", this);
    controlsLayout->addWidget(temperatureLabel, 5,0 );
    controlsLayout->addWidget(temperatureLE, 5, 1);
    controlsLayout->addWidget(roomT, 5, 2);
    controlsLayout->addWidget(nitrogenT, 5, 3);
    controlsLayout->addWidget(heliumT, 5, 4);
    connect(roomT, &QPushButton::clicked, this, &ListWidget::setRoomT);
    connect(nitrogenT, &QPushButton::clicked, this, &ListWidget::setNitrogenT);
    connect(heliumT, &QPushButton::clicked, this, &ListWidget::setHeliumT);

    controlsLayout->addWidget(testButton, 6, 0, 2, 5);

    vbox->setSpacing(10);
    vbox->addStretch(1);
    vbox->addWidget(save);
    vbox->addWidget(add);
    vbox->addWidget(rename);
    vbox->addWidget(remove);
    vbox->addWidget(removeAll);
    vbox->addStretch(1);

    hbox->addWidget(lw);
    hbox->addSpacing(15);

    hbox->addLayout(vbox);
    hbox->addLayout(controlsLayout);

    connect(testButton, &QPushButton::clicked, this, &ListWidget::testItem);
    connect(save, &QPushButton::clicked, this, &ListWidget::saveItem);
    connect(add, &QPushButton::clicked, this, &ListWidget::addItem);
    connect(rename, &QPushButton::clicked, this, &ListWidget::renameItem);
    connect(remove, &QPushButton::clicked, this, &ListWidget::removeItem);
    connect(removeAll, &QPushButton::clicked, this, &ListWidget::clearItems);

    setLayout(hbox);
}

void ListWidget::testItem()
{
    QMessageBox msgBox;
   
    QTextStream out(stdout);
    QString filename = "input.txt";
    QFile file(filename);
    for (int i = 0; i < lw->count(); ++i)
    {
        QString material_name = lw->item(i)->text();
        out << material_name << "\n";
        
        if (file.open(QIODevice::WriteOnly)) 
        {
            QTextStream out(&file);
            out << "iterations   1e+8\n";
            out << "nout   40000\n";
            out << "exstart   0\n";
            out << "iterations   3\n";
            out << "exend   1200\n";
            out << "lof   0\n";
            out << "gamma   1e+12\n";
            out << "temperature   77.00000000000000\n";
            out << "material   " + material_name + "\n";
            out << "limiterForce   0\n";
            out << "fieldInputConfig   1\n";
            out << "PolarInput   1\n";
            out << "KostilPolarConst   10\n";
            out << "triangleCheckConfig   1\n";
            out << "triangleCheckPause   0\n";
            out << "triangleCheckNumber   1.00001\n";
            out << "ee_collisions 1\n";
            out << "ee_rate 1e+12\n";
            //msgBox.setText("Items have been saved");
            file.close();
            view_debug("MC1.exe");
            view_debug("input.txt");
        }
    }
    msgBox.setText("START CALCULATIONS");
    msgBox.exec();
   // QString filename = "input.txt";

  //  QProcess* process = new QProcess(this);
   // view_debug("MC1.exe");
   // view_debug("input.txt");
  //  QString file = "MC1.exe";
//C:\Users\bagiz\Desktop\bin\NewGreshnovCalc\!QT_TEST
   // QProcess::startDetached(file);
    //process->start("C:/windows/system32/cmd.exe", QStringList() << "/C" << "C:/Users/bagiz/Desktop/bin/NewGreshnovCal/!QT_TEST/MC1.exe");
    //process->start(file);
   // if (process->atEnd()) {
    //    msgBox.setText("Done!");
   //     msgBox.exec();
   // }
    
}

void ListWidget::view_debug(const char* pszFileName)
{
    ShellExecuteA(GetDesktopWindow(), "open", pszFileName, NULL, NULL, SW_SHOW);
}


void ListWidget::saveItem()
{
    QTextStream out(stdout);
    QString filename = "config.txt";
    QFile file(filename);
    QMessageBox msgBox;
    if (file.open(QIODevice::WriteOnly)) {
        QTextStream out(&file);
        for (int i = 0; i < lw->count(); ++i)
        {

            QString str = lw->item(i)->text();
            out << str << "\n";
        }
        msgBox.setText("Items have been saved");
        file.close();
    }
    else msgBox.setText("Cannot create the file");
    msgBox.exec();
}

void ListWidget::addItem() {

    toCalcList.push_back(MaterialStruct(
        QVariant(materialCB->currentText()).toString(),
        iterationsSB->value(),
        maxFieldLE->text().toInt(),
        numberFieldsToCalcSB->value(),
        outputPountsLE->text().toInt(),
        gammaSB->value(),
        temperatureLE->text().toInt()
    ));
    lw->addItem(toCalcList.back().description);

   /* QString c_text = QInputDialog::getText(this, "Item", "Enter new item");
    QString s_text = c_text.simplified();

    if (!s_text.isEmpty()) {
        QString comboText = QVariant(materialCB->currentText()).toString();
        lw->addItem(s_text + " " + comboText
            + " " + QVariant(iterationsSB->value()).toString() 
            + " " + QVariant(temperatureLE->text()).toString() );
        int r = lw->count() - 1;
        lw->setCurrentRow(r);
    }*/
}

void ListWidget::renameItem() {

    QListWidgetItem* curitem = lw->currentItem();

    int r = lw->row(curitem);
    QString c_text = curitem->text();
    QString r_text = QInputDialog::getText(this, "Item",
        "Enter new item", QLineEdit::Normal, c_text);

    QString s_text = r_text.simplified();

    if (!s_text.isEmpty()) {

        QListWidgetItem* item = lw->takeItem(r);
        delete item;
        lw->insertItem(r, s_text);
        lw->setCurrentRow(r);
    }
}

void ListWidget::removeItem() {

    int r = lw->currentRow();

    if (r != -1) {

        QListWidgetItem* item = lw->takeItem(r);
        auto it = toCalcList.begin() + r;
        toCalcList.erase(it);
        delete item;
    }
}

void ListWidget::clearItems() {

    if (lw->count() != 0) {
        lw->clear();
    }
}

void ListWidget::setRoomT()
{
    temperatureLE->setText("300");
}

void ListWidget::setHeliumT() 
{
    temperatureLE->setText("4");
}

void ListWidget::setNitrogenT()
{
    temperatureLE->setText("77");
}
