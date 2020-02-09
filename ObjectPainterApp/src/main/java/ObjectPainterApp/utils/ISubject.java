package ObjectPainterApp.utils;

public interface ISubject {

    void addObserver(IObserver observer);
    void removeObserver(IObserver observer);
    void notifyObservers();

}
