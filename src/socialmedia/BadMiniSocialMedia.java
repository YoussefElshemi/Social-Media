package socialmedia;

import java.io.IOException;
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
			accounts.remove(account);
		} else {
			throw new AccountIDNotRecognisedException();
		}

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

		if (account != null) {
			account.setHandle(newHandle);
		} else {
			throw new HandleNotRecognisedException();
		}

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

		if (account != null) {
			builder.append("ID: " + account.getId());
			builder.append("Handle: " + account.getHandle());
		} else {
			throw new HandleNotRecognisedException();
		}
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

		if (account != null) {
			SocialMediaPost post = new SocialMediaPost(account, message);
			posts.add(post);
			return post.getId();
		} else {
			throw new HandleNotRecognisedException();
		}
	}

	@Override
	public int endorsePost(String handle, int id)
			throws HandleNotRecognisedException, PostIDNotRecognisedException, NotActionablePostException {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int commentPost(String handle, int id, String message) throws HandleNotRecognisedException,
			PostIDNotRecognisedException, NotActionablePostException, InvalidPostException {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public void deletePost(int id) throws PostIDNotRecognisedException {
		// TODO Auto-generated method stub

	}

	@Override
	public String showIndividualPost(int id) throws PostIDNotRecognisedException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public StringBuilder showPostChildrenDetails(int id)
			throws PostIDNotRecognisedException, NotActionablePostException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public int getMostEndorsedPost() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int getMostEndorsedAccount() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public void erasePlatform() {
		// TODO Auto-generated method stub

	}

	@Override
	public void savePlatform(String filename) throws IOException {
		// TODO Auto-generated method stub

	}

	@Override
	public void loadPlatform(String filename) throws IOException, ClassNotFoundException {
		// TODO Auto-generated method stub

	}

}
