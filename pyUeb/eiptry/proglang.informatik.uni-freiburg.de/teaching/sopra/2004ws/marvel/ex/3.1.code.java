class TpeakSlider 
{
    private JSlider slider;
    // private HIER_WAS_EINGEBEN burnPanel;
    // private HIER_WAS_EINGEBEN thrustPanel;
    // private HIER_WAS_EINGEBEN valueLabel;

    public TpeakSlider()
    {
	// HIER_WAS_EINGEBEN (INSTANTIIEREN DER PANELS UND DES LABELS)
	slider = new JSlider();
	sliderMax = slider.getMaximum();
	sliderMin = slider.getMinimum();
	slider.setValue(slider.getMinimum());
	// slider.addChangeListener(HIER_WAS_EINGEBEN);
    }
    
    public void stateChanged(ChangeEvent e)
    {
	double val = slider.getValue();
	double tp = (val - sliderMin) / (sliderMax - sliderMin);
	// burnPanel.HIER_WAS_EINGEBEN (HIER_WAS_EINGEBEN);
	// thrustPanel.HIER_WAS_EINGEBEN (HIER_WAS_EINGEBEN);
	// valueLabel.HIER_WAS_EINGEBEN (HIER_WAS_EINGEBEN);
    }
}



