import java.io.IOException;

import socialmedia.AccountIDNotRecognisedException;
import socialmedia.SocialMedia;
import socialmedia.HandleNotRecognisedException;
import socialmedia.IllegalHandleException;
import socialmedia.InvalidHandleException;
import socialmedia.InvalidPostException;
import socialmedia.NotActionablePostException;
import socialmedia.PostIDNotRecognisedException;
import socialmedia.SocialMediaPlatform;

/**
 * A short program to illustrate an app testing some minimal functionality of a
 * concrete implementation of the SocialMediaPlatform interface -- note you will
 * want to increase these checks, and run it on your SocialMedia class (not the
 * BadSocialMedia class).
 *
 * 
 * @author Diogo Pacheco
 * @version 1.0
 */
public class SocialMediaPlatformTestApp {

	/**
	 * Test method.
	 * 
	 * @param args not used
	 */
	public static void main(String[] args) {
		System.out.println("The system compiled and started the execution...");

		SocialMediaPlatform platform = new SocialMedia();

	  assert (platform.getNumberOfAccounts() == 0) : "Innitial SocialMediaPlatform not empty as required.";
		assert (platform.getTotalOriginalPosts() == 0) : "Innitial SocialMediaPlatform not empty as required.";
		assert (platform.getTotalCommentPosts() == 0) : "Innitial SocialMediaPlatform not empty as required.";
		assert (platform.getTotalEndorsmentPosts() == 0) : "Innitial SocialMediaPlatform not empty as required.";

    try {
			Integer id = platform.createAccount("youssef"); // create youssef profile
			assert (platform.getNumberOfAccounts() == 1) : "number of accounts registered in the system does not match.";
      
      platform.createAccount("lucian"); // create lucian profile
			assert (platform.getNumberOfAccounts() == 2) : "number of accounts registered in the system does not match.";

      Integer postId = platform.createPost("youssef", "my first post"); // youssef makes a post
      Integer commentId = platform.commentPost("lucian", postId, "replying to the post"); // lucian comments under the post
      platform.commentPost("youssef", commentId, "replying to the reply"); // youssef replies to lucian's comment
      platform.commentPost("youssef", postId, "replying to main post"); // youssef comments under the post
      platform.endorsePost("lucian", postId); // lucian endorses post

      assert (platform.getTotalOriginalPosts() == 1) : "number of original posts in the system does not match.";
		  assert (platform.getTotalCommentPosts() == 3) : "number of comments in the system does not match.";
		  assert (platform.getTotalEndorsmentPosts() == 1) : "number of endorsement posts in the system does not match";
      
      platform.savePlatform("./test.ser");
      platform.erasePlatform();
      platform.loadPlatform("./test.ser");
			assert (platform.getNumberOfAccounts() == 2) : "number of accounts registered in the system does not match";

      platform.removeAccount(id);
      platform.removeAccount("lucian");
			assert (platform.getNumberOfAccounts() == 0) : "number of accounts registered in the system does not match";
		} catch (IllegalHandleException e) {
			assert (false) : "IllegalHandleException thrown incorrectly";
		} catch (InvalidHandleException e) {
			assert (false) : "InvalidHandleException thrown incorrectly";
		} catch (AccountIDNotRecognisedException e) {
			assert (false) : "AccountIDNotRecognizedException thrown incorrectly";
    } catch (HandleNotRecognisedException e) {
			assert (false) : "HandleNotRecognisedException thrown incorrectly";
    } catch (InvalidPostException e) {
			assert (false) : "InvalidPostException thrown incorrectly";
    } catch (PostIDNotRecognisedException e) {
      assert (false) : "PostIDNotRecognisedException thrown incorrectly"; 
    } catch (NotActionablePostException e) {
      assert (false) : "NotActionablePostException thrown incorrectly"; 
    } catch (IOException e) {
      assert (false) : "IOException thrown incorrectly"; 
    } catch (ClassNotFoundException e) {
      assert (false) : "ClassNotFoundException thrown incorrectly"; 
    }
  }
}
