package socialmedia;

import java.io.*;
import java.util.*;

/**
 * BadSocialMedia is a minimally compiling, but non-functioning implementor of
 * the SocialMediaPlatform interface.
 * 
 * @author Diogo Pacheco
 * @version 1.0
 */
public class SocialMedia implements SocialMediaPlatform {

	private ArrayList<SocialMediaAccount> accounts = new ArrayList<SocialMediaAccount>();
	private ArrayList<SocialMediaPost> posts = new ArrayList<SocialMediaPost>();

	@Override
	public int createAccount(String handle) throws IllegalHandleException, InvalidHandleException {
    if (!accounts.isEmpty()) {
      for (SocialMediaAccount currentAccount:accounts) {
        if (currentAccount.getHandle().equals(handle)) {
          throw new IllegalHandleException();
        }
      }
    }

		SocialMediaAccount account = new SocialMediaAccount(handle);
		accounts.add(account);
		return account.getId();
	}

	@Override
	public int createAccount(String handle, String description) throws IllegalHandleException, InvalidHandleException {
	  for (SocialMediaAccount currentAccount:accounts) {
			if (currentAccount.getHandle().equals(handle)) {
				throw new IllegalHandleException();
			}
		}

		SocialMediaAccount account = new SocialMediaAccount(handle, description);
		accounts.add(account);
		return account.getId();
	}

	@Override
	public void removeAccount(int id) throws AccountIDNotRecognisedException {
		SocialMediaAccount account = null;

		for (SocialMediaAccount currentAccount:accounts) {
			if (currentAccount.getId() == id) {
				account = currentAccount;
			}
		}

		if (account == null) {
			throw new AccountIDNotRecognisedException();
		}

		accounts.remove(account);
	}

	@Override
	public void removeAccount(String handle) throws HandleNotRecognisedException {
		SocialMediaAccount account = null;

		for (SocialMediaAccount currentAccount:accounts) {
			if (currentAccount.getHandle().equals(handle)) {
				account = currentAccount;
			}
		}

		if (account == null) {
			throw new HandleNotRecognisedException();
		}

		accounts.remove(account);

	}

	@Override
	public void changeAccountHandle(String oldHandle, String newHandle) throws HandleNotRecognisedException, IllegalHandleException, InvalidHandleException {
		SocialMediaAccount account = null;

		for (SocialMediaAccount currentAccount:accounts) {
			if (currentAccount.getHandle().equals(oldHandle)) {
				account = currentAccount;
			}
		}

		if (account == null) {
			throw new HandleNotRecognisedException();
		}

		account.setHandle(newHandle);
	}

	@Override
	public void updateAccountDescription(String handle, String description) throws HandleNotRecognisedException {
		SocialMediaAccount account = null;

		for (SocialMediaAccount currentAccount:accounts) {
			if (currentAccount.getHandle().equals(handle)) {
				account = currentAccount;
			}
		}

		if (account == null) {
			throw new HandleNotRecognisedException();
		}

		account.setDescription(description);
	}

	@Override
	public String showAccount(String handle) throws HandleNotRecognisedException {
		StringBuilder builder = new StringBuilder();
		SocialMediaAccount account = null;

		for (SocialMediaAccount currentAccount:accounts) {
			if (currentAccount.getHandle().equals(handle)) {
				account = currentAccount;
			}
		}

		if (account == null) {
			throw new HandleNotRecognisedException();
		}
			
		builder.append("ID: " + account.getId() + "\n");
		builder.append("Handle: " + account.getHandle() + "\n");
		builder.append("Description: " + account.getDescription() + "\n");
		builder.append("Post count: " + account.getPosts().size() + "\n");
		builder.append("Endorse count: " + account.getEndorsedPosts().size() + "\n");
		
		return builder.toString();
	}

	@Override
	public int createPost(String handle, String message) throws HandleNotRecognisedException, InvalidPostException {
		SocialMediaAccount account = null;

		for (SocialMediaAccount currentAccount:accounts) {
			if (currentAccount.getHandle().equals(handle)) {
				account = currentAccount;
			}
		}

		if (account == null) {
			throw new HandleNotRecognisedException();
		}
		
		SocialMediaPost post = new SocialMediaPost(account, message);
		posts.add(post);
		account.getPosts().add(post);

		return post.getId();
	}

	@Override
	public int endorsePost(String handle, int id) throws HandleNotRecognisedException, PostIDNotRecognisedException, NotActionablePostException {
		SocialMediaAccount account = null;
		SocialMediaPost post = null;

		for (SocialMediaAccount currentAccount:accounts) {
			if (currentAccount.getHandle().equals(handle)) {
				account = currentAccount;
			}
		}

		for (SocialMediaPost currentPost:posts) {
			if (currentPost.getId() == id) {
				post = currentPost;
			}
		}

		if (account == null) {
			throw new HandleNotRecognisedException();
		}

		if (post == null) {
			throw new PostIDNotRecognisedException();
		}

		if (Boolean.TRUE.equals(post.getEndorsed())) {
			throw new NotActionablePostException();
		}

		String message = "EP@" + account.getHandle() + ": " + post.getMessage();
		SocialMediaPost endorsedPost = new SocialMediaPost(message);

		post.getEndorsements().add(endorsedPost);
		posts.add(endorsedPost);
		account.getPosts().add(endorsedPost);
		account.getEndorsedPosts().add(endorsedPost);

		return endorsedPost.getId();
	}

	@Override
	public int commentPost(String handle, int id, String message) throws HandleNotRecognisedException, PostIDNotRecognisedException, NotActionablePostException, InvalidPostException {
		SocialMediaAccount account = null;
		SocialMediaPost post = null;

		for (SocialMediaAccount currentAccount:accounts) {
			if (currentAccount.getHandle().equals(handle)) {
				account = currentAccount;
			}
		}

		for (SocialMediaPost currentPost:posts) {
			if (currentPost.getId() == id) {
				post = currentPost;
			}
		}

		if (account == null) {
			throw new HandleNotRecognisedException();
		}

		if (post == null) {
			throw new PostIDNotRecognisedException();
		}

		if (Boolean.TRUE.equals(post.getEndorsed())) {
			throw new NotActionablePostException();
		}

		SocialMediaPost comment = new SocialMediaPost(account, message);
		posts.add(comment);
		post.getComments().add(comment);
		comment.setParent(post);

		return comment.getId();
	}

	@Override
	public void deletePost(int id) throws PostIDNotRecognisedException {
		SocialMediaPost post = null;

		for (SocialMediaPost currentPost:posts) {
			if (currentPost.getId() == id) {
				post = currentPost;
			}
		}

		if (post == null) {
			throw new PostIDNotRecognisedException();
		}

		posts.remove(post);
		post.getAccount().getPosts().remove(post);

		for (SocialMediaPost endorsedPost:post.getEndorsements()) {
			deletePost(endorsedPost.getId());
		}

		String message = "The original content was removed from the system and is no longer available.";
		SocialMediaPost genericPost = new SocialMediaPost(message);

		for (SocialMediaPost comment:post.getComments()) {
			comment.setParent(genericPost);
		}
	}

	@Override
	public String showIndividualPost(int id) throws PostIDNotRecognisedException {
		StringBuilder builder = new StringBuilder();
		SocialMediaPost post = null;

		for (SocialMediaPost currentPost:posts) {
			if (currentPost.getId() == id) {
				post = currentPost;
			}
		}

		if (post == null) {
			throw new PostIDNotRecognisedException();
		}
			
		builder.append("ID: " + post.getId() + "\n");
		builder.append("Account: " + post.getAccount().getHandle() + "\n");
		builder.append("No. endorsements: " + post.getEndorsements().size() + " | No. comments: " + post.getComments().size() + "\n");
		builder.append(post.getMessage() + "\n");
		
		return builder.toString();
	}

	@Override
	public StringBuilder showPostChildrenDetails(int id) throws PostIDNotRecognisedException, NotActionablePostException {
		StringBuilder builder = new StringBuilder();
    SocialMediaPost post = null;

		for (SocialMediaPost currentPost:posts) {
			if (currentPost.getId() == id) {
				post = currentPost;
			}
		}

		if (post == null) {
			throw new PostIDNotRecognisedException();
		}

    if (Boolean.TRUE.equals(post.getEndorsed())) {
      throw new NotActionablePostException();
    }

    builder.append(showIndividualPost(post.getId()));

    for (SocialMediaPost currentPost:post.getComments()) {
      String newLine = showPostChildrenDetails(currentPost.getId()).toString();
      String[] lines = newLine.split("\n");
      for (String line:lines) {
        builder.append("     " + line + "\n");
      }
    }
		
		return builder;
  }


	@Override
	public int getNumberOfAccounts() {
		return accounts.size();
	}

	@Override
	public int getTotalOriginalPosts() {
		int counter = 0;
		for (SocialMediaPost post:posts) {
			if (Boolean.FALSE.equals(post.getEndorsed()) && post.getParent() == null) {
				counter++;
			}
		}
		return counter;
	}

	@Override
	public int getTotalEndorsmentPosts() {
		int counter = 0;
		for (SocialMediaPost post:posts) {
			if (Boolean.TRUE.equals(post.getEndorsed())) {
				counter++;
			}
		}
		return counter;
	}

	@Override
	public int getTotalCommentPosts() {
		int counter = 0;
		for (SocialMediaPost post:posts) {
			if (post.getParent() != null) {
				counter++;
			}
		}
		return counter;
	}

	@Override
	public int getMostEndorsedPost() {
		SocialMediaPost mostEndorsedPost = posts.get(0);
		for (SocialMediaPost post:posts) {
			if (post.getEndorsements().size() > mostEndorsedPost.getEndorsements().size()) {
				mostEndorsedPost = post;
			}
		}

		return mostEndorsedPost.getId();
	}

	@Override
	public int getMostEndorsedAccount() {
		SocialMediaAccount mostEndorsedAccount = accounts.get(0);
		for (SocialMediaAccount account:accounts) {
			if (account.getEndorsedPosts().size() > mostEndorsedAccount.getEndorsedPosts().size()) {
				mostEndorsedAccount = account;
			}
		}

		return mostEndorsedAccount.getId();
	}

	@Override
	public void erasePlatform() {
		accounts.clear();
		posts.clear();

	}

	@Override
	public void savePlatform(String filename) throws IOException {
		FileOutputStream fos = new FileOutputStream(filename);
    ObjectOutputStream oos = new ObjectOutputStream(fos);
    oos.writeObject(this);
    oos.close();
    fos.close();
	}

	@Override
	public void loadPlatform(String filename) throws IOException, ClassNotFoundException {
		FileInputStream fis = new FileInputStream(filename);
		ObjectInputStream ois = new ObjectInputStream(fis);
		SocialMedia badSocialMedia = (SocialMedia)ois.readObject();
		this.accounts = badSocialMedia.accounts;
		this.posts = badSocialMedia.posts;

		ois.close();
		fis.close();
	}

}
