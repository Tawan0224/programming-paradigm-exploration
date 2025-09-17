import java.util.*;

public class BookingSystem {
    private final String[] slots;

    public BookingSystem(int size) {
        if (size <= 0) throw new IllegalArgumentException("size must be positive");
        slots = new String[size];
    }

    // Book a slot if it is free
    public boolean book(int index, String name) {
        checkIndex(index);
        if (slots[index] == null) {
            slots[index] = name;
            return true;
        } else {
            return false;
        }
    }

    // Cancel a booking
    public boolean cancel(int index) {
        checkIndex(index);
        if (slots[index] != null) {
            slots[index] = null;
            return true;
        } else {
            return false;
        }
    }

    // Display slot status
    public void show() {
        System.out.println("Slots:");
        for (int i = 0; i < slots.length; i++) {
            String s = (slots[i] == null) ? "FREE" : ("BOOKED by " + slots[i]);
            System.out.printf("  [%d] %s%n", i, s);
        }
    }

    private void checkIndex(int idx) {
        if (idx < 0 || idx >= slots.length) {
            throw new IndexOutOfBoundsException("Index out of range: " + idx);
        }
    }

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);

        System.out.print("Number of slots to create: ");
        int n;
        try {
            n = Integer.parseInt(sc.nextLine().trim());
        } catch (Exception e) {
            System.out.println("Invalid number. Defaulting to 5.");
            n = 5;
        }

        BookingSystem system = new BookingSystem(n);
        System.out.println("Commands: show | book <index> <name> | cancel <index> | exit");

        while (true) {
            System.out.print("> ");
            String line = sc.nextLine().trim();
            if (line.isEmpty()) continue;
            String[] parts = splitArgs(line);
            String cmd = parts[0].toLowerCase(Locale.ROOT);

            try {
                switch (cmd) {
                    case "show":
                        system.show();
                        break;
                    case "book":
                        if (parts.length < 3) {
                            System.out.println("Usage: book <index> <name>");
                            break;
                        }
                        int idx = Integer.parseInt(parts[1]);
                        // join remaining as name
                        String name = join(parts, 2);
                        boolean ok = system.book(idx, name);
                        System.out.println(ok ? "Booked." : "Slot already booked.");
                        break;
                    case "cancel":
                        if (parts.length != 2) {
                            System.out.println("Usage: cancel <index>");
                            break;
                        }
                        int cidx = Integer.parseInt(parts[1]);
                        boolean canceled = system.cancel(cidx);
                        System.out.println(canceled ? "Canceled." : "Slot already free.");
                        break;
                    case "exit":
                    case "quit":
                        System.out.println("Bye.");
                        sc.close();
                        return;
                    default:
                        System.out.println("Unknown command.");
                }
            } catch (IndexOutOfBoundsException ex) {
                System.out.println("Error: " + ex.getMessage());
            } catch (NumberFormatException ex) {
                System.out.println("Invalid index number.");
            } catch (Exception ex) {
                System.out.println("An error occurred: " + ex.getMessage());
            }
        }
    }

    private static String[] splitArgs(String line) {
        return line.trim().split("\\s+");
    }

    private static String join(String[] parts, int start) {
        StringBuilder sb = new StringBuilder();
        for (int i = start; i < parts.length; i++) {
            if (i > start) sb.append(' ');
            sb.append(parts[i]);
        }
        return sb.toString();
    }
}




/**
 * Simple Booking System (Java)
 * - Slots are indexed from 0 .. n-1
 * - Each slot can be free (null) or booked with a customer name (String)
 *
 * Run:
 *   javac BookingSystem.java
 *   java BookingSystem
 *
 * Commands in the console:
 *   show
 *   book <index> <name>
 *   cancel <index>
 *   exit
 */
