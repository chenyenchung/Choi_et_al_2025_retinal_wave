Dialog.create("Enlarge ROI");
Dialog.addMessage("How much do you want to enlarge?\n(Negative value to shirnk)");
Dialog.addNumber("By:", "");
Dialog.show();
enlarge_dia = Dialog.getNumber();
counts=roiManager("count");
for(i=0; i<counts; i++) {
    roiManager("Select", i);
    enlarge_param = "enlarge=" + enlarge_dia;
    run("Enlarge...", enlarge_param);
    roiManager("Update");
}