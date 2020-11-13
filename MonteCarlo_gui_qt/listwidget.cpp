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
    
    lw = new QListWidget(this);
    loadConfig();
   
    QVBoxLayout* vbox = new QVBoxLayout();
    vbox->setSpacing(10);
    //QVBoxLayout* vbox2 = new QVBoxLayout();
    //vbox2->setSpacing(10);
    QGridLayout* controlsLayout = new QGridLayout;

    QHBoxLayout* hbox = new QHBoxLayout(this);


    save = new QPushButton("Save", this);
    add = new QPushButton("Add", this);
    rename = new QPushButton("Rename", this);
    remove = new QPushButton("Remove", this);
    removeAll = new QPushButton("Remove All", this);
    runButton = new QPushButton("RUN", this);
    
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

    controlsLayout->addWidget(runButton, 6, 0, 2, 5);

    vbox->setSpacing(10);
    vbox->addStretch(1);
    vbox->addWidget(add);
    vbox->addWidget(save);
    vbox->addWidget(rename);
    vbox->addWidget(remove);
    vbox->addWidget(removeAll);
    vbox->addStretch(1);

    hbox->addWidget(lw);
    hbox->addSpacing(15);

    hbox->addLayout(vbox);
    hbox->addLayout(controlsLayout);

    connect(runButton, &QPushButton::clicked, this, &ListWidget::runItem);
    connect(save, &QPushButton::clicked, this, &ListWidget::saveItem);
    connect(add, &QPushButton::clicked, this, &ListWidget::addItem);
    connect(rename, &QPushButton::clicked, this, &ListWidget::renameItem);
    connect(remove, &QPushButton::clicked, this, &ListWidget::removeItem);
    connect(removeAll, &QPushButton::clicked, this, &ListWidget::clearItems);

    setLayout(hbox);
}

bool ListWidget::loadConfig()
{
    clearItems();
    QString filename = "config.txt";
    QFile file(filename);
    if (!file.open(QIODevice::ReadOnly)) {
        qWarning("Cannot open file for reading"); 
        return false;
    }
    QTextStream in(&file);
    while (!in.atEnd()) {
        MaterialStruct materialStructTemp;
        materialStructTemp.material = in.readLine();
        materialStructTemp.description = in.readLine();
        materialStructTemp.iterations = in.readLine().toInt();
        materialStructTemp.maxField = in.readLine().toInt();
        materialStructTemp.fieldToCalc = in.readLine().toInt();
        materialStructTemp.outPoints = in.readLine().toInt();
        materialStructTemp.gamma = in.readLine().toInt();
        materialStructTemp.temp = in.readLine().toInt();
        toCalcList.push_back(materialStructTemp);
    }
    for (auto const& i : toCalcList) lw->addItem(i.description);
    return true;
}

bool ListWidget::saveConfig()
{
    saveItem();
    return false;
}

void ListWidget::runItem()
{
    QMessageBox msgBox;
    if (toCalcList.size() == 0)
    {
        msgBox.setText("Add processes to calculate");
        msgBox.exec();
        return;
    }
    if (cores < toCalcList.size())
    {
        const QMessageBox::StandardButton ret
            = QMessageBox::warning(this, tr("MC gui"),
                "You are trying to start more processes <b>(" + QString::number(toCalcList.size()) +
                ")</b> then availiable cores <b>(" + QString::number(cores) +")</b>\n"
                    "It may decrease performance of each process\n"
                    "Do you want to run all?",
                QMessageBox::Yes | QMessageBox::No);
        if (ret == QMessageBox::No) return;
    }
    QString forMsgBox;
   
    QTextStream out(stdout);
    QString filename = "input.txt";
    QFile file(filename);
    //for (int i = 0; i < lw->count(); ++i)
    for (auto const &i : toCalcList)
    {
        //QString material_name = lw->item(i)->text();
        //out << material_name << "\n";
        
        if (file.open(QIODevice::WriteOnly)) 
        {
            QTextStream out(&file);
            out << "iterations   " + QString::number(pow(10, i.iterations)) + "\n";
            out << "nout   " + QString::number(i.outPoints) + "\n";
            out << "exstart   0\n";
            out << "iterations   " + QString::number(i.fieldToCalc) + "\n";
            out << "exend   " + QString::number(i.maxField) + "\n";
            out << "lof   0\n";
            out << "gamma  " + QString::number(pow(10,i.gamma)) +"\n";
            out << "temperature   "+ QString::number(i.temp) +"\n";
            out << "material   " + i.material + "\n";
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
            //view_debug("MC1.exe");
            //view_debug("input.txt");
            ShellExecuteA(GetDesktopWindow(), "open", "input.txt", NULL, NULL, SW_SHOW);
            ShellExecuteA(GetDesktopWindow(), "open", "MC1.exe", NULL, NULL, SW_SHOW);
            forMsgBox += i.description + "\n";
        }
    }
    msgBox.setText("START CALCULATIONS\n"+forMsgBox);
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
        for (auto const &i : toCalcList)
        {
            out << i.material << "\n";
            out << i.description << "\n";
            out << i.iterations << "\n";
            out << i.maxField << "\n";
            out << i.fieldToCalc << "\n";
            out << i.outPoints << "\n";
            out << i.gamma << "\n";
            out << i.temp << "\n";
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
        //auto it = toCalcList.begin() + r;
        toCalcList.erase(toCalcList.begin() + r);
        delete item;
    }
}

void ListWidget::clearItems() {

    toCalcList.clear();
    if (lw->count() != 0) {
        lw->clear();
    }
}

void ListWidget::setRoomT() {temperatureLE->setText("300");}
void ListWidget::setHeliumT() {temperatureLE->setText("4");}
void ListWidget::setNitrogenT(){temperatureLE->setText("77");}
