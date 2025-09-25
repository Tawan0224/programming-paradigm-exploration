import java.util.Scanner;

public class BookingSystem {
    private String[] slots;
    
    //Create a booking system with specified number of slots
    public BookingSystem(int size) {
        slots = new String[size];
    }
    
    //Book a slot for a person
    public void book(int index, String name) {
        if (slots[index] == null) {
            slots[index] = name;
            System.out.println("Booked!");
        } else {
            System.out.println("Already booked!");
        }
    }

    //Cancel a booking and make slot free
    public void cancel(int index) {
        if (slots[index] != null) {
            slots[index] = null; 
            System.out.println("Canceled!");
        } else {
            System.out.println("Already free!");
        }
    }

    //Display all slots and their status
    public void show() {
        for (int i = 0; i < slots.length; i++) {
            if (slots[i] == null) {
                System.out.println(i + ": FREE");
            } else {
                System.out.println(i + ": " + slots[i]);
            }
        }
    }
    
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        
        // Ask user for number of slots
        System.out.print("How many slots do you want? ");
        int numSlots = sc.nextInt();
        
        // Create booking system with user-specified slots
        BookingSystem booking = new BookingSystem(numSlots);
        
        System.out.println("Created " + numSlots + " slots (0 to " + (numSlots-1) + ")");
        System.out.println("Commands: show, book, cancel, exit");
        
        while (true) {
            System.out.print("> ");
            String cmd = sc.next();
            
            if (cmd.equals("show")) {
                booking.show();
            } else if (cmd.equals("book")) {
                int index = sc.nextInt();
                String name = sc.next();
                booking.book(index, name);
            } else if (cmd.equals("cancel")) {
                int index = sc.nextInt();
                booking.cancel(index);
            } else if (cmd.equals("exit")) {
                break;
            }
        }
        sc.close();
    }
}