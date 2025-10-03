import java.util.Scanner;

public class BookingSystem {
    private String[][] classes;  // 2D array: [class][slot]
    private int[] classSizes;     // Store size of each class
    
    // Create a booking system with specified number of classes
    public BookingSystem(int numClasses) {
        classes = new String[numClasses][];
        classSizes = new int[numClasses];
    }
    
    // Initialize a specific class with a given number of slots
    public void initClass(int classIndex, int slots) {
        classes[classIndex] = new String[slots];
        classSizes[classIndex] = slots;
    }
    
    // Book a slot for a person in a specific class
    public void book(int classIndex, int slotIndex, String name) {
        if (classIndex < 0 || classIndex >= classes.length || classes[classIndex] == null) {
            System.out.println("Invalid class!");
            return;
        }
        if (slotIndex < 0 || slotIndex >= classes[classIndex].length) {
            System.out.println("Invalid slot!");
            return;
        }
        
        // Check if person already has a booking in this class
        for (int i = 0; i < classes[classIndex].length; i++) {
            if (classes[classIndex][i] != null && classes[classIndex][i].equals(name)) {
                System.out.println("Error: " + name + " already has a booking in class " + classIndex + "!");
                return;
            }
        }
        
        // Book the slot if it's free
        if (classes[classIndex][slotIndex] == null) {
            classes[classIndex][slotIndex] = name;
            System.out.println("Booked!");
        } else {
            System.out.println("Already booked!");
        }
    }

    // Cancel a booking and make slot free
    public void cancel(int classIndex, int slotIndex) {
        if (classIndex < 0 || classIndex >= classes.length || classes[classIndex] == null) {
            System.out.println("Invalid class!");
            return;
        }
        if (slotIndex < 0 || slotIndex >= classes[classIndex].length) {
            System.out.println("Invalid slot!");
            return;
        }
        
        if (classes[classIndex][slotIndex] != null) {
            classes[classIndex][slotIndex] = null; 
            System.out.println("Canceled!");
        } else {
            System.out.println("Already free!");
        }
    }

    // Display all classes and their slots
    public void show() {
        for (int c = 0; c < classes.length; c++) {
            if (classes[c] == null) {
                System.out.println("Class " + c + ": Not initialized");
                continue;
            }
            System.out.println("Class " + c + ":");
            for (int s = 0; s < classes[c].length; s++) {
                if (classes[c][s] == null) {
                    System.out.println("  Slot " + s + ": FREE");
                } else {
                    System.out.println("  Slot " + s + ": " + classes[c][s]);
                }
            }
        }
    }

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        
        // Ask user for number of classes
        System.out.print("How many classes do you want? ");
        int numClasses = sc.nextInt();
        
        // Create booking system with user-specified classes
        BookingSystem booking = new BookingSystem(numClasses);
        
        // Initialize each class with different sizes
        for (int i = 0; i < numClasses; i++) {
            System.out.print("How many slots for class " + i + "? ");
            int slots = sc.nextInt();
            booking.initClass(i, slots);
        }
        
        System.out.println("\nCreated " + numClasses + " classes");
        System.out.println("Commands: show, book, cancel, exit");
        
        while (true) {
            System.out.print("> ");
            String cmd = sc.next();
            
            if (cmd.equals("show")) {
                booking.show();
            } else if (cmd.equals("book")) {
                int classIndex = sc.nextInt();
                int slotIndex = sc.nextInt();
                String name = sc.next();
                booking.book(classIndex, slotIndex, name);
            } else if (cmd.equals("cancel")) {
                int classIndex = sc.nextInt();
                int slotIndex = sc.nextInt();
                booking.cancel(classIndex, slotIndex);
            } else if (cmd.equals("exit")) {
                break;
            }
        }
        sc.close();
    }
}


/*
 * javac BookingSystem.java
 * java BookingSystem
 * 
 * Example Usage:
 * How many classes do you want? 2
 * How many slots for class 0? 3
 * How many slots for class 1? 2
 * 
 * Created 2 classes
 * Commands: show, showclass, book, cancel, exit
 * 
 * > show
 * Class 0:
 *   Slot 0: FREE
 *   Slot 1: FREE
 *   Slot 2: FREE
 * Class 1:
 *   Slot 0: FREE
 *   Slot 1: FREE
 * 
 * > book 0 1 Trump
 * Booked!
 * 
 * > book 0 2 Trump
 * Error: Trump already has a booking in class 0!
 * 
 * > book 1 0 Trump
 * Booked!
 * 
 * > show
 * Class 0:
 *   Slot 0: FREE
 *   Slot 1: Trump
 *   Slot 2: FREE
 * Class 1:
 *   Slot 0: Trump
 *   Slot 1: FREE
 * 
 * > cancel 0 1
 * Canceled!
 * 
 * > showclass 0
 * Class 0:
 *   Slot 0: FREE
 *   Slot 1: FREE
 *   Slot 2: FREE
 * 
 * > exit
 */