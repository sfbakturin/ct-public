import java.util.concurrent.locks.ReentrantLock;

/**
 * Bank implementation.
 *
 * <p>:TODO: This implementation has to be made thread-safe.
 *
 * @author :TODO: Bakturin Saveliy
 */
public class BankImpl implements Bank {
    /**
     * An array of accounts by index.
     */
    private final Account[] accounts;

    /**
     * Creates new bank instance.
     * @param n the number of accounts (numbered from 0 to n-1).
     */
    public BankImpl(int n) {
        accounts = new Account[n];
        for (int i = 0; i < n; i++) {
            accounts[i] = new Account();
        }
    }

    @Override
    public int getNumberOfAccounts() {
        return accounts.length;
    }

    /**
     * <p>:TODO: This method has to be made thread-safe.
     */
    @Override
    public long getAmount(int index) {
        accounts[index].lock.lock();
        final long ret = accounts[index].amount;
        accounts[index].lock.unlock();
        return ret;
    }

    /**
     * <p>:TODO: This method has to be made thread-safe.
     */
    @Override
    public long getTotalAmount() {
        for (final Account a : accounts) {
            a.lock.lock();
        }
        long sum = 0;
        for (Account account : accounts) {
            sum += account.amount;
        }
        for (final Account a : accounts) {
            a.lock.unlock();
        }
        return sum;
    }

    /**
     * <p>:TODO: This method has to be made thread-safe.
     */
    @Override
    public long deposit(int index, long amount) {
        accounts[index].lock.lock();
        if (amount <= 0) {
            accounts[index].lock.unlock();
            throw new IllegalArgumentException("Invalid amount: " + amount);
        }
        Account account = accounts[index];
        if (amount > MAX_AMOUNT || account.amount + amount > MAX_AMOUNT) {
            accounts[index].lock.unlock();
            throw new IllegalStateException("Overflow");
        }
        account.amount += amount;
        final long ret = account.amount;
        accounts[index].lock.unlock();
        return ret;
    }

    /**
     * <p>:TODO: This method has to be made thread-safe.
     */
    @Override
    public long withdraw(int index, long amount) {
        accounts[index].lock.lock();
        if (amount <= 0) {
            accounts[index].lock.unlock();
            throw new IllegalArgumentException("Invalid amount: " + amount);
        }
        Account account = accounts[index];
        if (account.amount - amount < 0) {
            accounts[index].lock.unlock();
            throw new IllegalStateException("Underflow");
        }
        account.amount -= amount;
        final long ret = account.amount;
        accounts[index].lock.unlock();
        return ret;
    }

    /**
     * <p>:TODO: This method has to be made thread-safe.
     */
    @Override
    public void transfer(int fromIndex, int toIndex, long amount) {
        accounts[Math.min(fromIndex, toIndex)].lock.lock();
        accounts[Math.max(toIndex, fromIndex)].lock.lock();
        final int indexFst = Math.min(fromIndex, toIndex);
        final int indexSnd = Math.max(toIndex, fromIndex);
        if (amount <= 0) {
            accounts[indexSnd].lock.unlock();
            accounts[indexFst].lock.unlock();
            throw new IllegalArgumentException("Invalid amount: " + amount);
        }
        if (fromIndex == toIndex) {
            accounts[indexSnd].lock.unlock();
            accounts[indexFst].lock.unlock();
            throw new IllegalArgumentException("fromIndex == toIndex");
        }
        Account from = accounts[fromIndex];
        Account to = accounts[toIndex];
        if (amount > from.amount) {
            accounts[indexSnd].lock.unlock();
            accounts[indexFst].lock.unlock();
            throw new IllegalStateException("Underflow");
        }
        else if (amount > MAX_AMOUNT || to.amount + amount > MAX_AMOUNT) {
            accounts[indexSnd].lock.unlock();
            accounts[indexFst].lock.unlock();
            throw new IllegalStateException("Overflow");
        }
        from.amount -= amount;
        to.amount += amount;
        accounts[indexSnd].lock.unlock();
        accounts[indexFst].lock.unlock();
    }

    /**
     * Private account data structure.
     */
    static class Account {
        /**
         * Amount of funds in this account.
         */
        long amount;

        /**
         * Synchonized lock.
         */
        private final ReentrantLock lock;

        /**
         * Creates a new account instance.
         */
        public Account() {
            lock = new ReentrantLock();
        }
    }
}
