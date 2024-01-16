package de.uni_freiburg.informatik.proglang.inetprog.ex3;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import javax.activation.DataHandler;
import javax.activation.DataSource;
import javax.activation.FileDataSource;
import javax.activation.MimetypesFileTypeMap;
import javax.mail.BodyPart;
import javax.mail.Multipart;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.event.TransportAdapter;
import javax.mail.event.TransportEvent;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeMultipart;


/*
 * Created on 13.05.2005
 *
 */

/**
 * @author Christian Kössler, Stefan Wehr
 * Programm dient dem Senden einer Email via JavaMail.
 * 
 * Bevor die Email versendet wird, wird nach Benutzername und Passwort gefragt (PopupAuthenticator Klasse). 
 */
public class MailClient {
    
    private String m_smtpHost;
    
    public MailClient(String smtpHost) {
        m_smtpHost = smtpHost;
    }
    
    public void sendMail(String from, String to, String subject, String text, String[][] attachments) {
        try {
            Properties props = System.getProperties();
            props.put("mail.smtp.host", m_smtpHost);
            Session session = Session.getDefaultInstance(props);
            MimeMessage msg = new MimeMessage(session);
            msg.setFrom(new InternetAddress(from));
            msg.setRecipient(MimeMessage.RecipientType.TO,new InternetAddress(to));
            
            /**
             * Hier wird der Inhalt festgelegt.
             */
            msg.setSubject(subject);
            Multipart multipart = new MimeMultipart("mixed"); // Container fuer die einzelnen Teile der Mail
            
            // Text 
            BodyPart messageBodyPart = new MimeBodyPart();
            messageBodyPart.setText(text);
            multipart.addBodyPart(messageBodyPart);
            
            // Fileattachments
            for (String[] alternatives : attachments) {
                if (alternatives.length == 1) {
                    String filename = alternatives[0];
                    BodyPart attachment = new MimeBodyPart();
                    DataSource source = new FileDataSource(filename);
                    attachment.setDataHandler(new DataHandler(source));
                    attachment.setFileName(filename);
                    multipart.addBodyPart(attachment);
                } else {
                    MimeMultipart subpart = new MimeMultipart("alternative");
                    for (String filename : alternatives) {
                        MimeBodyPart alternative = new MimeBodyPart();
                        DataSource source = new FileDataSource(filename);
                        alternative.setDataHandler(new DataHandler(source));
                        alternative.setFileName(filename);
                        subpart.addBodyPart(alternative);
                    }
                    MimeBodyPart wrap = new MimeBodyPart();
                    wrap.setContent(subpart);    // HERE'S THE KEY
                    multipart.addBodyPart(wrap);
                }
            }
            
            msg.setContent(multipart);
   
            /**
             * Im folgenden verfolgen wir den Status unserer Mail.
             */
            Transport trans = session.getTransport("smtp");
            trans.addTransportListener(new TransportAdapter() {
                public void messageDelivered(TransportEvent arg0) {
                    System.out.println("Delivered!");
                }
                public void messageNotDelivered(TransportEvent arg0) {
                    System.out.println("Not delivered!");
                }
                public void messagePartiallyDelivered(TransportEvent arg0) {
                    System.out.println("Partialy delivered!");
                }
            });
            
            /**
             * Absenden der Mail
             */
            trans.connect();
            trans.sendMessage(msg,msg.getAllRecipients());
            trans.close();
            
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static String[][] parseAlternatives(String[] arr) {
        List<String[]> l = new ArrayList<String[]>();
        for (String s : arr) {
            String[] splitted = s.split("\\|");
            l.add(splitted);
        }
        return l.toArray(new String[0][]);
    }
    
    public static void main(String[] args) {
        if (args.length < 5) {
            System.err.println("Illegal commandline arguments, expected: SMTP-HOST FROM TO SUBJECT TEXT FILENAMES");
            System.exit(1);
        }
        try {
            String smtpHost = args[0]; 
            String from = args[1];
            String to = args[2];
            String subject = args[3];
            String text = args[4];
            String[] restArgs = new String[args.length - 5];
            System.arraycopy(args, 5, restArgs, 0, restArgs.length);
            MailClient client = new MailClient(smtpHost);
            client.sendMail(from, to, subject, text, parseAlternatives(restArgs));
        } catch(Exception e) {
            System.err.println(e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }
}
