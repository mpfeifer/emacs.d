package PACKAGE;

import java.util.List;

// Import log4j classes.
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;


class CLASSNAME {

    // Define a static logger variable
    private static final Logger logger = LogManager.getLogger(CLASSNAME.class);

    public static void main(final String... args) {
        logger.trace("Entering application.");

        logger.error("Application logic not yet implemented.")

        logger.trace("Exiting application.");
    }

}
