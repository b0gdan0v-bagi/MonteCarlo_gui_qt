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

ListWidget::ListWidget(QWidget* parent)
    : QWidget(parent) {
    
    QVBoxLayout* vbox = new QVBoxLayout();
    vbox->setSpacing(10);
    QVBoxLayout* vbox2 = new QVBoxLayout();
    vbox2->setSpacing(10);

    QHBoxLayout* hbox = new QHBoxLayout(this);

    lw = new QListWidget(this);
    lw->addItem("si");
    lw->addItem("inas");


    save = new QPushButton("Save", this);
    add = new QPushButton("Add", this);
    rename = new QPushButton("Rename", this);
    remove = new QPushButton("Remove", this);
    removeAll = new QPushButton("Remove All", this);
    testButton = new QPushButton("TEST", this);
    
    // material;
    QHBoxLayout* materialBox = new QHBoxLayout();
    materialLabel = new QLabel("Material :");
    materialCB = new QComboBox(this);
    materialCB->addItem("ge");
    materialCB->addItem("si");
    materialCB->addItem("inas");
    materialCB->addItem("ingaas");
    materialCB->addItem("cdhgte");
    materialBox->addWidget(materialLabel);
    materialBox->addWidget(materialCB);
    vbox2->addLayout(materialBox);
    // iterations
    QHBoxLayout* iterationsBox = new QHBoxLayout();
    iterationsSB = new QSpinBox(this);
    iterationsSB->setMinimum(3);
    iterationsSB->setMaximum(7);
    iterationsSB->setValue(4);
    iterationsLabel = new QLabel("test my own label = 10^");
    iterationsBox->addWidget(iterationsLabel);
    iterationsBox->addWidget(iterationsSB);
    vbox2->addLayout(iterationsBox);
    // max field
    QHBoxLayout* maxFieldBox = new QHBoxLayout();
    maxFieldLabel = new QLabel("max field, V/sm");
    maxFieldLE = new QLineEdit(this);
    maxFieldLE->setValidator(new QIntValidator(1, 1000000, this));
    maxFieldLE->setText(QString::number(1200));
    numberFieldsToCalcLabel = new QLabel("fields to calculate");
    numberFieldsToCalcSB = new QSpinBox(this);
    numberFieldsToCalcSB->setMinimum(2);
    numberFieldsToCalcSB->setMaximum(10);
    numberFieldsToCalcSB->setValue(3);
    maxFieldBox->addWidget(maxFieldLabel);
    maxFieldBox->addWidget(maxFieldLE);
    maxFieldBox->addWidget(numberFieldsToCalcLabel);
    maxFieldBox->addWidget(numberFieldsToCalcSB);
    vbox2->addLayout(maxFieldBox);
    // out points
    QHBoxLayout* outPointsBox = new QHBoxLayout();
    outputPointsLabel = new QLabel("Number of output points ");
    outputPountsLE = new QLineEdit(this);
    outputPountsLE->setValidator(new QIntValidator(1, 50000, this));
    outputPountsLE->setText(QString::number(40000));
    outPointsBox->addWidget(outputPointsLabel);
    outPointsBox->addWidget(outputPountsLE);
    vbox2->addLayout(outPointsBox);
    // gamma
    QHBoxLayout* gammaBox = new QHBoxLayout();
    gammaLabel = new QLabel("gamma = 10^");
    gammaSB = new QSpinBox(this);
    gammaSB->setMinimum(8);
    gammaSB->setMaximum(15);
    gammaSB->setValue(12);
    gammaBox->addWidget(gammaLabel);
    gammaBox->addWidget(gammaSB);
    vbox2->addLayout(gammaBox);
    // tempterature
    QHBoxLayout* temperatureBox = new QHBoxLayout();
    temperatureLabel = new QLabel(tr("Temperature, K"));
    temperatureLE = new QLineEdit(this);
    temperatureLE->setValidator(new QIntValidator(0,400,this));
    temperatureLE->setText(QString::number(77));
    roomT = new QPushButton("room", this);
    nitrogenT = new QPushButton("nitro", this);
    heliumT = new QPushButton("hellium", this);
    temperatureBox->addWidget(temperatureLabel);
    temperatureBox->addWidget(temperatureLE);
    temperatureBox->addWidget(roomT);
    temperatureBox->addWidget(nitrogenT);
    temperatureBox->addWidget(heliumT);
    connect(roomT, &QPushButton::clicked, this, &ListWidget::setRoomT);
    connect(nitrogenT, &QPushButton::clicked, this, &ListWidget::setNitrogenT);
    connect(heliumT, &QPushButton::clicked, this, &ListWidget::setHeliumT);
    vbox2->addLayout(temperatureBox);
    //vbox2-> setSpacing(10);
    vbox2->addWidget(testButton);

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
    //myGrid->addLayout(vbox, 0, 0);
    //myGrid->addLayout(vbox2, 1, 1);
    //myGrid->addLayout(layout , 2, 2);
    hbox->addLayout(vbox);

    //hbox->addStretch(1);
    hbox->addLayout(vbox2);


   // connect(testSlider, &QSlider::, this, &ListWidget::addItem);

    connect(testButton, &QPushButton::clicked, this, &ListWidget::testItem);
    connect(save, &QPushButton::clicked, this, &ListWidget::saveItem);
    connect(add, &QPushButton::clicked, this, &ListWidget::addItem);
    connect(rename, &QPushButton::clicked, this, &ListWidget::renameItem);
    connect(remove, &QPushButton::clicked, this, &ListWidget::removeItem);
    connect(removeAll, &QPushButton::clicked, this, &ListWidget::clearItems);

    setLayout(hbox);
    //setLayout(myGrid);

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

    QString c_text = QInputDialog::getText(this, "Item", "Enter new item");
    QString s_text = c_text.simplified();

    if (!s_text.isEmpty()) {
        QString comboText = QVariant(materialCB->currentText()).toString();
        lw->addItem(s_text + " " + comboText
            + " " + QVariant(iterationsSB->value()).toString() 
            + " " + QVariant(temperatureLE->text()).toString() );
        int r = lw->count() - 1;
        lw->setCurrentRow(r);
    }
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
