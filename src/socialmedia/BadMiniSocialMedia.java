package socialmedia;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;

/**
 * BadMiniSocialMedia is a minimally compiling, but non-functioning implementor
 * of the MiniSocialMediaPlatform interface.
 * 
 * @author Diogo Pacheco
 * @version 1.0
 */
public class BadMiniSocialMedia implements MiniSocialMediaPlatform {

	private ArrayList<SocialMediaAccount> accounts = new ArrayList<>();
	private ArrayList<SocialMediaPost> posts = new ArrayList<>();

	@Override
	public int createAccount(String handle) throws IllegalHandleException, InvalidHandleException {
		for (SocialMediaAccount currentAccount:accounts) {
			if (currentAccount.getHandle().equals(handle)) {
				throw new IllegalHandleException();
			}
		}

		SocialMediaAccount account = new SocialMediaAccount(handle);
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

		if (account != null) {
			throw new AccountIDNotRecognisedException();
		}

		accounts.remove(account);
	}

	@Override
	public void changeAccountHandle(String oldHandle, String newHandle)
			throws HandleNotRecognisedException, IllegalHandleException, InvalidHandleException {
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
			
		builder.append("ID: " + account.getId());
		builder.append("Handle: " + account.getHandle());
		
		
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
	public int endorsePost(String handle, int id)
			throws HandleNotRecognisedException, PostIDNotRecognisedException, NotActionablePostException {
		
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
		SocialMediaPost endorsedPost = new SocialMediaPost(account, message);

		post.getEndorsements().add(endorsedPost);
		posts.add(endorsedPost);
		account.getPosts().add(endorsedPost);
		account.getEndorsedPosts().add(endorsedPost);

		return endorsedPost.getId();
	}

	@Override
	public int commentPost(String handle, int id, String message) throws HandleNotRecognisedException,
			PostIDNotRecognisedException, NotActionablePostException, InvalidPostException {
		
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
			
		builder.append("ID: " + post.getId());
		builder.append("Account: " + post.getAccount().getHandle());
		builder.append("No. endorsements: " + post.getEndorsements().size() + "| No. comments: " + post.getComments().size());
		builder.append(post.getMessage());
		
		return builder.toString();
	}

	@Override
	public StringBuilder showPostChildrenDetails(int id)
			throws PostIDNotRecognisedException, NotActionablePostException {
		StringBuilder builder = new StringBuilder();

		return builder;
	}

	@Override
	public int getMostEndorsedPost() {
		SocialMediaPost mostEndorsedPost = null;
		for (SocialMediaPost post:posts) {
			if (post.getEndorsements().size() > mostEndorsedPost.getEndorsements().size()) {
				mostEndorsedPost = post;
			}
		}

		return mostEndorsedPost.getId();
	}

	@Override
	public int getMostEndorsedAccount() {
		SocialMediaAccount mostEndorsedAccount = null;
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
		BadMiniSocialMedia badMiniSocialMedia = (BadMiniSocialMedia)ois.readObject();
		this.accounts = badMiniSocialMedia.accounts;
		this.posts = badMiniSocialMedia.posts;

		ois.close();
		fis.close();
	}

}
