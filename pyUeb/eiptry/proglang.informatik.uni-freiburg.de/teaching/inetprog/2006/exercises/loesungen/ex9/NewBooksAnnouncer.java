package proglang.j2ee.clients;

import java.io.IOException;
import java.text.DateFormat;
import java.util.Date;
import java.util.Hashtable;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.ObjectMessage;
import javax.jms.Session;
import javax.jms.Topic;
import javax.jms.TopicConnection;
import javax.jms.TopicConnectionFactory;
import javax.jms.TopicSession;
import javax.jms.TopicSubscriber;
import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;

public class NewBooksAnnouncer implements MessageListener
{
	public static void main(String[] args)
	{
		NewBooksAnnouncer announcer = new NewBooksAnnouncer();
		announcer.connectToTopic();
	}

	public void connectToTopic()
	{
		// Now notify the topic that a new book has been added.
		Context ctx = null;
		Hashtable<String, String> ht = new Hashtable<String, String>();
		ht.put(Context.INITIAL_CONTEXT_FACTORY, "weblogic.jndi.WLInitialContextFactory");
		ht.put(Context.PROVIDER_URL, "t3://nonopapa.informatik.uni-freiburg.de:7001");	

		
		try
		{
			ctx = new InitialContext(ht);
			TopicConnectionFactory factory = (TopicConnectionFactory) ctx.lookup("weblogic.jms.ConnectionFactory");
			TopicConnection connect = factory.createTopicConnection();
            Topic topic = (Topic) ctx.lookup("NewBooksTopic");
			TopicSession session = connect.createTopicSession(false, Session.AUTO_ACKNOWLEDGE);
			TopicSubscriber subscriber = session.createSubscriber(topic);
			connect.start();
			subscriber.setMessageListener(this);
		}
		catch (NamingException e)
		{
            System.err.println("NamingException: " + e.getMessage());
            System.err.println("Cause: " + e.getCause());
			e.printStackTrace();
		}
		catch (JMSException e)
		{
            System.err.println("JMSException: " + e.getMessage());
			e.printStackTrace();
		}
		finally
		{
			try
			{
				ctx.close();
			}
			catch (Exception e)
			{
				// a failure occurred
			}
		}
        System.out.println("Listening... (press any key to exit)");
        try {
            System.in.read();
        } catch (IOException e) {
        }
	}

	public void onMessage(Message arg0)
	{
		DateFormat df = DateFormat.getTimeInstance(DateFormat.LONG);
		try
		{
			ObjectMessage msg = (ObjectMessage) arg0;
			System.out.println("[" + df.format(new Date()) + "]: A new book has been added to the bookshop:");
			System.out.println(msg.getObject());
			arg0.acknowledge();
		}
		catch (JMSException jmse)
		{
			jmse.printStackTrace();
		}		
		
	}
}
